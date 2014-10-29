extern crate lexer;
extern crate nester;

use lexer::{Lexer, Other, Identifier, Char};
use nester::{nesting, Token, Tree, Tok};
use std::io::{BufferedReader, File};
use std::path::Path;
use std::collections::{HashSet, HashMap};

#[deriving(Show)]
enum RuleItem {
	Sym(String),
	Chr(char)
}

#[deriving(Show)]
struct Rule {
	seq: Vec<RuleItem>,
	rule: Token
}

#[deriving(Show)]
struct NTerm {
	type_: Vec<Token>,
	rules: Vec<Rule>
}

#[deriving(Show)]
struct Grammar {
	prelude: Vec<Token>,
	terms: HashSet<String>,
	chars: HashSet<char>,
	nterms: HashMap<String, NTerm>,
	start: Option<String>
}

fn parse_rules(tree: &[Token]) -> Vec<Rule> {
	let mut pos = 0;
	let mut rules: Vec<Rule> = Vec::new();
	loop {
		let mut seq: Vec<RuleItem> = Vec::new();
		loop {
			match tree[pos].content {
				Tok(Identifier(ref s)) => {
					seq.push(Sym(s.clone()));
					pos += 1;
				},
				Tok(Char(c)) => {
					seq.push(Chr(c));
					pos += 1;
				},
				Tree('}', _) => {
					rules.push(Rule {
						seq: seq,
						rule: tree[pos].clone()
					});
					pos += 1;
					break;
				},
				_ => fail!()
			}
		}
		if pos == tree.len() {
			return rules;
		}
	}
}

fn parse_grammar(tree: &[Token]) -> Grammar {
	let mut grammar = Grammar {
		prelude: Vec::new(),
		terms: HashSet::new(),
		chars: HashSet::new(),
		nterms: HashMap::new(),
		start: None
	};
	let mut pos = 0;
	loop {
		println!("*");
		match match tree[pos].content {
			Tok(ref x) => x,
			Tree(..) => fail!()
		} {
			&Other('#') => {
				pos += 1;
				match tree[pos].content {
					Tree('}', ref t @ _) => {
						grammar.prelude.push_all(t.as_slice());
					},
					Tok(Identifier(ref id)) => if !grammar.terms.insert(id.clone()) { fail!() },
					_ => fail!()
				}
				pos += 1;
			},
			&Identifier(ref name) => {
				pos += 1;
				let type_ = match tree[pos].content {
					Tree('}', _) => Vec::new(),
					Tree(')', ref x @ _) => {
						pos +=1;
						x.clone()
					},
					_ => fail!()
				};
				let rules = match tree[pos].content {
					Tree('}', ref t @ _) => {
						pos += 1;
						parse_rules(t.as_slice())
					},
					_ => fail!()
				};
				grammar.nterms.insert(name.clone(), NTerm {
					type_: type_,
					rules: rules
				});
			},
			_ => fail!()
		}
		if pos == tree.len() {
			break;
		}
	}
	grammar
}

fn main() {
	let path = Path::new("data/grammar");
	let file = match File::open(&path) {
		Ok(x) => x,
		Err(e) => fail!("file could not be opened: {}", e)
	};
	let lex = match Lexer::new(BufferedReader::new(file)) {
		Ok(x) => x,
		Err(e) => fail!("lexer could not be initialised: {}", e)
	};
	let tree = match nesting(lex, "data/grammar".to_string()) {
		Ok(x) => x,
		Err(e) => fail!("{}", e)
	};
	println!("{}", tree);
	println!("{}", parse_grammar(tree.as_slice()));
}