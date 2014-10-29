extern crate lexer;
extern crate nester;

use lexer::{Lexer, Other, Identifier};
use nester::{nesting, Token, Tree, Tok};
use std::io::{BufferedReader, File};
use std::path::Path;
use std::collections::{HashSet, HashMap};

struct Rule {
	seq: Vec<String>,
	rule: Token
}

struct Grammar {
	prelude: Vec<Token>,
	terms: HashSet<String>,
	chars: HashSet<char>,
	nterms: HashMap<String, Vec<Rule>>,
	start: Option<String>
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
			},
			&Identifier(_) => {
				fail!();
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
}