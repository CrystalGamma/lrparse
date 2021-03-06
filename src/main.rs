/*
    lrparse - An LR parser generator for Rust
    Copyright (C) 2014  Jona Stubbe

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
#![feature(globs)]

extern crate lexer;

use lexer::Lexer;
use nester::{nesting, Token, PrettyPrint};
use std::io::{BufferedReader, File};
use std::path::Path;
use std::collections::{HashSet, HashMap};
use std::sync::Arc;
use RuleItem::*;

mod nester;

/// a symbol in the broader sense (includes character symbols)
#[deriving(Show,PartialEq,Hash,Eq,Clone)]
enum RuleItem {
	Sym(String),	// a named symbol
	Chr(char)
}

impl RuleItem {
	pub fn is_eof(&self) -> bool {
		match self {
			&RuleItem::Chr(_) => false,
			&RuleItem::Sym(ref s) => s.as_slice() == "$eof"
		}
	}
}

/// a rule for how to construct a nonterminal symbol
struct Rule {
	seq: Vec<RuleItem>,
	code: Token,
	nterm: Arc<String>
}

impl Rule {
	/// formats the rule in a human-friendly form, optinally writing a position indicator ( . ) at a given position
	pub fn fmt_internal(&self, fmt: &mut std::fmt::Formatter, pos: Option<uint>)
			-> Result<(), std::fmt::Error> {
		match fmt.write_str(self.nterm.deref().as_slice()) {Err(_) => return Err(std::fmt::Error), _ => {}};
		match fmt.write_str(" <=") {Err(_) => return Err(std::fmt::Error), _ => {}};
		let mut idx = 0;
		if self.seq.len() > 0 {
			for item in self.seq.iter() {
				match pos {
					Some(x) if x == idx => {
						match fmt.write_str(" .") {Err(_) => return Err(std::fmt::Error), _ => {}};
					},
					_ => {}
				}
				idx += 1;
				try!(write!(fmt, " {}", item));
			}
		} else {
			match fmt.write_str(" [empty]") {Err(_) => return Err(std::fmt::Error), _ => {}};
		}
		Ok(())
	}
}

impl std::fmt::Show for Rule {
	fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		self.fmt_internal(fmt, None)
	}
}

/// collection of information on a nonterminal symbol
#[deriving(Show)]
struct NTerm {
	type_: Vec<Token>,
	rules: (uint, uint)
}

#[deriving(Show)]
struct Grammar {
	prelude: Vec<Token>,
	terms: HashMap<String, Vec<Token>>,
	chars: HashSet<char>,
	nterms: HashMap<String, NTerm>,
	rules: Vec<Rule>,
	errors: HashMap<String, Vec<Token>>
}

/// an item of state for a node
#[deriving(PartialEq, Eq, Show)]
struct RulePos(uint, uint);

/// a state node for the LR parser
#[deriving(Show)]
struct Node {
	state: Vec<RulePos>,
	shifts: HashMap<RuleItem, uint>,
	reduce: Option<uint>
}

mod parse_grammar;
mod node_graph;
mod output;
mod show;

/// creates a mapping between symbols and symbol IDs and warns about unused symbols
fn assign_numbers<'a, I: Iterator<&'a Node>>(mut nodes: I, grammar: &Grammar) -> (HashMap<RuleItem, uint>, uint) {
	let mut cur_id = 2u;
	let mut ids: HashMap<RuleItem, uint> = HashMap::new();
	ids.insert(Sym("$eof".to_string()), 0);
	ids.insert(Sym("Accept_".to_string()), 1);
	for node in nodes {
		for (item, _) in node.shifts.iter() {
			println!("found {}", item);
			if !ids.contains_key(item) {
				ids.insert(item.clone(), cur_id);
				println!("{}: {}", item, cur_id);
				cur_id += 1;
			}
		}
	}
	for (term, _) in grammar.terms.iter() {
		if !ids.contains_key(&Sym(term.clone())) {
			println!("Warning: unused terminal symbol {}", term);
		}
	}
	for (nterm, _) in grammar.nterms.iter() {
		if !ids.contains_key(&Sym(nterm.clone())) {
			println!("Warning: unused non-terminal symbol {}", nterm);
		}
	}
	(ids, cur_id)
}

/// parses the command-line arguments
///
/// result: (input, output, debug-mode parser?, log level)
fn parse_cmdline<'a, I: 'a + Iterator<&'a String>>(mut args: I) -> (String, String, bool, uint) {
	#[deriving(PartialEq, Show)]
	enum State {
		Output,
		Nothing
	}

	let mut state = State::Nothing;
	let mut gramm = "data/grammar".to_string();
	let mut out = "out.rs".to_string();
	let mut debug = false;
	let mut loglevel = 0;
	let mut have_input = false;
	let mut have_output = false;
	for arg in args {
		match state {
			State::Nothing => {
				if arg.char_at(0) == '-' {
					if arg.char_at(1) == '-' {
						match arg.as_slice() {
							"--debug" => { debug = true; },
							"--verbose" | "--verbose=1" => { loglevel = 1; },
							"--verbose=2" => { loglevel = 2; },
							"--verbose=3" => { loglevel = 3; },
							"--out" | "--output" => { state = State::Output; },
							_ => panic!("unrecognised cmdline argument {}", arg)
						}
					} else { // single-char args
						for c in arg.slice_from(1).chars() {
							match c {
								'o' => { state = State::Output; },
								'd' => { debug = true; },
								'v' => { loglevel += 1; },
								_ => panic!("unrecognised cmdline option -{}", c)
							}
						}
					}
				} else {
					gramm = arg.clone();
					if have_input {
						println!("Warning: Two input files specified");
					} else {
						have_input = true;
					}
				}
			},
			State::Output => {
				state = State::Nothing;
				out = arg.clone();
				if have_output {
					println!("Warning: Two output files specified");
				} else {
					have_output = true;
				}
			}
		}
	}
	if state != State::Nothing {
		panic!("expecting {} at the end of command line arguments", state);
	}
	(gramm, out, debug, loglevel)
}

fn main() {
	let args = std::os::args();
	let (gramm, out, debug, log_level) = parse_cmdline(args.slice_from(1).iter());
	let path = Path::new(gramm.as_slice());
	let file = match File::open(&path) {
		Ok(x) => x,
		Err(e) => panic!("file could not be opened: {}", e)
	};
	let lex = match Lexer::new(BufferedReader::new(file)) {
		Ok(x) => x,
		Err(e) => panic!("lexer could not be initialised: {}", e)
	};
	let tree = match nesting(lex, "data/grammar".to_string()) {
		Ok(x) => x,
		Err(e) => panic!("{}", e)
	};
	if log_level > 2 {
		match tree.iter().pretty_print(&mut std::io::stdio::stdout(), 0, true) {
			Ok(_) => {},
			Err(e) => panic!("{}", e)
		}
	}
	let grammar = parse_grammar::parse_grammar(tree.as_slice(), log_level);
	if log_level > 2 {
		println!("{}", grammar);
	}
	let nodes = node_graph::create_nodes(&grammar, log_level);
	if log_level > 2 {
		println!("{}", nodes);
	}
	let (mapping, num_symbols) = assign_numbers(nodes.iter(), &grammar);
	if log_level > 2 {
		println!("mapping: {}", mapping);
	}
	match output::write_parser(&Path::new(out.as_slice()), nodes, mapping, num_symbols, &grammar, debug) {
		Ok(()) => {},
		Err(e) => panic!("could not write grammar: {}", e)
	}
}