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
extern crate lexer;

use lexer::Lexer;
use nester::{nesting, Token, PrettyPrint};
use std::io::{BufferedReader, File};
use std::path::Path;
use std::collections::{HashSet, HashMap};
use std::sync::Arc;
use parse_grammar::parse_grammar;
use node_graph::create_nodes;
use output::write_parser;

mod nester;

#[deriving(Show,PartialEq,Hash,Eq,Clone)]
enum RuleItem {
	Sym(String),
	Chr(char)
}

#[deriving(Show)]
struct Rule {
	seq: Vec<RuleItem>,
	code: Token,
	nterm: Arc<String>
}

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

#[deriving(Show)]
struct Node {
	state: Vec<(uint,uint)>,	// (rule, position)
	shifts: HashMap<RuleItem, uint>,
	reduce: Option<uint>
}

mod parse_grammar;
mod node_graph;
mod output;

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

static mut LOG_LEVEL: uint = 0u;

fn log_level() -> uint {
	unsafe {
		LOG_LEVEL
	}
}

fn main() {
	let args = std::os::args();
	#[deriving(PartialEq, Show)]
	enum State {
		Output,
		Nothing
	}
	let mut state = Nothing;
	let mut gramm = "data/grammar".to_string();
	let mut out = "out.rs".to_string();
	for arg in args.iter() {
		match state {
			Nothing => {
				match arg.as_slice() {
					"-o" => { state = Output; },
					_ => { gramm = arg.clone(); }
				}
			},
			Output => {
				state = Nothing;
				out = arg.clone();
			}
		}
	}
	if state != Nothing {
		panic!("expecting {} at the end of command line arguments", state);
	}
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
	if log_level() > 2 {
		match tree.iter().pretty_print(&mut std::io::stdio::stdout(), 0, true) {
			Ok(_) => {},
			Err(e) => panic!("{}", e)
		}
	}
	let grammar = parse_grammar(tree.as_slice());
	if log_level() > 2 {
		println!("{}", grammar);
	}
	let nodes = create_nodes(&grammar);
	if log_level() > 2 {
		println!("{}", nodes);
	}
	let (mapping, num_symbols) = assign_numbers(nodes.iter(), &grammar);
	if log_level() > 2 {
		println!("mapping: {}", mapping);
	}
	match write_parser(&Path::new(out.as_slice()), nodes, mapping, num_symbols, &grammar, true) {
		Ok(()) => {},
		Err(e) => panic!("could not write grammar: {}", e)
	}
}