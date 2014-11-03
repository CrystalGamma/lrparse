extern crate lexer;
extern crate nester;

use lexer::{Lexer, Other, Identifier, Char, Arrow};
use nester::{nesting, Token, Tree, Tok, PrettyPrint, CodeReference};
use std::io::{BufferedReader, File, Write, Truncate, IoResult};
use std::path::Path;
use std::collections::{HashSet, HashMap};
use std::sync::Arc;

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
	type_: Token,
	rules: (uint, uint)
}

#[deriving(Show)]
struct Grammar {
	prelude: Vec<Token>,
	terms: HashMap<String, Token>,
	chars: HashSet<char>,
	nterms: HashMap<String, NTerm>,
	rules: Vec<Rule>
}

fn parse_rules(tree: &[Token], nterm: &Arc<String>, grammar: &mut Grammar) -> (uint, uint) {
	let mut pos = 0;
	let start = grammar.rules.len();
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
					grammar.rules.push(Rule {
						seq: seq,
						code: tree[pos].clone(),
						nterm: nterm.clone()
					});
					pos += 1;
					break;
				},
				Tok(Arrow) => { 
					pos += 1;
				}
				_ => panic!()
			}
		}
		println!("rule {}: {}", grammar.rules.len() - 1, grammar.rules.last());
		if pos != tree.len() {
			match tree[pos].content {
				Tok(Other(',')) => { pos += 1; },
				_ => {}
			}
		}
		if pos == tree.len() {
			return (start, grammar.rules.len());
		}
	}
}

fn parse_grammar(tree: &[Token]) -> Grammar {
	let mut grammar = Grammar {
		prelude: Vec::new(),
		terms: HashMap::new(),
		chars: HashSet::new(),
		nterms: HashMap::new(),
		rules: Vec::new(),
	};
	grammar.nterms.insert("Accept_".to_string(), NTerm {
		type_: Token {
			content: Tree('}', Vec::new()),
			ref_: CodeReference::internal()
		},
		rules: (0u, 1u)
	});
	grammar.rules.push(Rule {
		seq: Vec::new(),
		code: Token {
			content: Tree('}', Vec::new()),	// TODO
			ref_: CodeReference::internal()
		},
		nterm: Arc::new("Accept_".to_string())
	});
	let mut pos = 0;
	let mut startsymbol = true;
	loop {
		println!("*");
		match match tree[pos].content {
			Tok(ref x) => x,
			Tree(..) => panic!()
		} {
			&Other('#') => {
				pos += 1;
				match tree[pos].content {
					Tree('}', ref t @ _) => {
						grammar.prelude.push_all(t.as_slice());
						pos += 1;
					},
					Tok(Identifier(ref id)) => {
						pos += 1;
						match tree[pos].content {
							Tree(')', _) => if !grammar.terms.insert(id.clone(), tree[pos].clone()) {
								panic!() 
							} else {
								pos += 1;
							},
							_ => if !grammar.terms.insert(id.clone(), Token {
								content: Tree('}', Vec::new()),
								ref_: CodeReference::internal()
							}) {
								panic!();
							}
						}
					}
					_ => panic!()
				}
			},
			&Identifier(ref name) => {
				pos += 1;
				let arc = Arc::new(name.clone());
				let type_ = match tree[pos].content {
					Tree('}', _) => Token {
						content: Tree('}', Vec::new()),	// TODO
						ref_: CodeReference::internal()
					},
					Tree(')',  _) => {
						pos +=1;
						tree[pos-1].clone()
					},
					_ => panic!()
				};
				match tree[pos].content {
					Tree('}', ref t @ _) => {
						pos += 1;
						if startsymbol {
							match grammar.nterms.find_mut(&"Accept_".to_string()) {
								Some(x) => { x.type_ = type_.clone(); }
								None => panic!()
							}
							grammar.rules[0].seq = vec![Sym(name.clone())];
							startsymbol = false;
						}
						let nterm = NTerm {
							type_: type_,
							rules: parse_rules(t.as_slice(), &arc, &mut grammar)
						};
						grammar.nterms.insert(name.clone(), nterm);
					},
					_ => panic!()
				}
			},
			_ => panic!()
		}
		if pos != tree.len() {
			match tree[pos].content {
				Tok(Other(',')) => { pos += 1; },
				_ => {}
			}
		}
		if pos == tree.len() {
			break;
		}
	}
	grammar
}

#[deriving(Show)]
struct Node {
	state: Vec<(uint,uint)>,	// (rule, position)
	shifts: HashMap<RuleItem, uint>,
	reduce: Option<uint>
}

fn fill_up_state(state: &mut Vec<(uint, uint)>, grammar: &Grammar) {
	let mut idx = 0;
	while idx < state.len() {	// we want to append to the array as we go, so no iterator :(
		let (r, pos) = state[idx];
		idx += 1;
		let rule = &grammar.rules[r];
		if rule.seq.len() > pos {
			match rule.seq[pos] {
				Chr(_) => {},
				Sym(ref s) => {
					match grammar.nterms.find(s) {
						Some(ref sym) => {
							let (start, end) = sym.rules;
							for i in range(start,end) {
								if !state.contains(&(i, 0u)) {
									state.push((i, 0u));
								}
							}
						}
						None => {
							match grammar.terms.find(s) {
								Some(_) => {},
								None => panic!("rule {} ({}) refers to unknown symbol {}",
										r, rule, s)
							}
						}
					}
				}
			}
		}
	}
}

fn derive_node(grammar: &Grammar, pos: uint, item: RuleItem, nodes: &mut Vec<Node>) {
	print!("deriving by {}: ", item);
	let mut newstate: Vec<(uint,uint)> = (*nodes)[pos].state.iter().filter(|&&(rule, p): &&(uint, uint)| {
			let r = &grammar.rules[rule];
			if r.seq.len() <= p {
				return false;
			}
			r.seq[p] == item
		}).map(|&(rule, p): &(uint, uint)| (rule, p + 1)).collect();
	fill_up_state(&mut newstate, grammar);
	if nodes.iter().any(|nod: &Node| {
		if nod.state.len() != newstate.len() {
			return false;
		}
		for state in nod.state.iter() {
			if !newstate.contains(state) {
				return false;
			}
		}
		true
	}) {
		return;
	}
	let idx = nodes.len();
	nodes[pos].shifts.insert(item, idx);
	println!("{}", newstate);
	nodes.push(Node {
		state: newstate,
		shifts: HashMap::new(),
		reduce: None
	});
}

fn create_nodes(grammar: &Grammar) -> Vec<Node> {
	let mut nodes: Vec<Node> = Vec::new();
	let mut st = vec![(0,0)];
	fill_up_state(&mut st, grammar);
	nodes.push(Node {
		state: st,
		shifts: HashMap::new(),
		reduce: None
	});
	let start = grammar.rules[0].seq[0].clone();
	derive_node(grammar, 0, start, &mut nodes);	// enforce the accept state always being node #1
	let mut pos = 0u;	// can't use iterator here because we write to the vector
	loop {
		let mut shifts: HashSet<RuleItem> = HashSet::new();
		{
		let node = &mut nodes[pos];
		println!("Node {}: {}", pos, node);
		for &(rule, p) in node.state.iter() {
			let r = &grammar.rules[rule];
			if r.seq.len() > p {
				println!("rule {}, shift by {}", rule, r.seq[p]);
				shifts.insert(r.seq[p].clone());
			} else {
				match node.reduce {
					None => { node.reduce = Some(rule); }
					Some(x) => panic!("Reduce-Reduce conflict in node {} ({}) between rule {} ({}) and {} ({})",
						pos, node, x, grammar.rules[x], rule, r)
				}
			}
		}
		println!("-> shifts: {}", shifts);
		match node.reduce {
			None => {},
			Some(x) => println!("-> reduce: {} ({})", x, grammar.rules[x])
		}
		}
		for item in shifts.into_iter() {
			derive_node(grammar, pos, item, &mut nodes);
		}
		pos += 1;
		if pos == nodes.len() {
			return nodes;
		}
	}
}

fn assign_numbers(nodes: &Vec<Node>, grammar: &Grammar) -> (HashMap<RuleItem, uint>, uint) {
	let mut cur_id = 2u;
	let mut ids: HashMap<RuleItem, uint> = HashMap::new();
	ids.insert(Sym("$eof".to_string()), 0);
	ids.insert(Sym("Accept_".to_string()), 1);
	for node in nodes.iter() {
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

fn write_parser(filename: &Path, nodes: Vec<Node>, mapping: HashMap<RuleItem, uint>, num_symbols: uint, grammar: &Grammar)
		-> IoResult<()> {
	let mut file = try!(File::open_mode(filename, Truncate, Write));
	let out = &mut file;
	try!(grammar.prelude.iter().pretty_print(out, 0, false));
	try!(out.write_str("struct Token {
		Other(char)"));
	for (item, _) in mapping.iter() {
		if item == &Sym("$eof".to_string()) {
			continue;
		}
		match item {
			&Chr(_) => {},
			&Sym(ref s) => {
				println!("{}", s);
				try!(out.write_str(",\n\t"));
				try!(out.write_str(s.as_slice()));
				match grammar.nterms.find(s) {
					Some(ref x) => try!(x.type_.pretty_print_token(out, 1)),
					None => try!(grammar.terms[*s].pretty_print_token(out, 1))
				}
			}
		}
	}
	try!(out.write_str("}
static TABLE: &'static [uint] = &["));
	let num_rules = grammar.rules.len();
	for node in nodes.into_iter() {
		let mut line: Vec<uint> = Vec::with_capacity(num_symbols);
		line.grow(num_symbols, match node.reduce {
			Some(x) => x + 1,
			None => 0u
		});
		for (shift, target) in node.shifts.into_iter() {
			let tok_id = mapping[shift];
			match shift {
				Chr(_) => { line[tok_id] = target + num_rules + 1; continue; },
				Sym(ref s) => match grammar.nterms.find(s) {
					None => { line[tok_id] = target + num_rules + 1; }
					Some(_) => { line[tok_id] = target; }
				}
			}
			
		}
		println!("{}", line);
		try!(out.write_str("\n"));
		for num in line.into_iter() {
			try!(format_args!(|args: &std::fmt::Arguments| out.write_fmt(args), "{:8u},", num));
		}
	}
	try!(format_args!(|args: &std::fmt::Arguments| out.write_fmt(args),
		"];\nstatic NUM_RULES: uint = {}u;\nstatic NUM_SYMBOLS: uint = {}u;\n", grammar.rules.len(), num_symbols));
	try!(out.write_str("
#[deriving(Show)]
pub struct Parser {
	stack: Vec<(uint, Token)>,
}

impl Parser {
	pub fn new() -> Parser {
		Parser {stack: vec![(0u, Other('#'))]}
	}
	fn get_token_id(tok: &Token) -> uint {
		match tok {"));
	for (item, &id) in mapping.iter() {
		match item {
			&Chr(c @ _) => {
				try!(out.write_str("\n\t\t\t&Other"));
				try!(Token::internal(Tree(')',
					vec![Token::internal(Tok(Char(c)))])).pretty_print_token(out, 1));
				try!(format_args!(|args: &std::fmt::Arguments| out.write_fmt(args),
					" => {},", id));
			},
			&Sym(ref s) if id != 0 => {
				try!(format_args!(|args: &std::fmt::Arguments| out.write_fmt(args),
					"\n\t\t\t&{}(_) => {},", s, id));
			}
			_ => {}
		}
	}
	try!(out.write_str("\n\t\t\t_ => panic!(\"unknown token used in parser\")
		}
	}
	pub fn consume_token(&mut self, tok: Token) -> Result<(),()> {
		let tok_id = Parser::get_token_id(&tok);
		try!(self.do_reduces(tok_id));
		let &(state, _) = match self.stack.last() {Some(x) => x, None => panic!()};
		self.stack.push((TABLE[state*NUM_SYMBOLS + tok_id]-NUM_RULES-1, tok));
		Ok(())
	}
	fn do_reduces(&mut self, tok_id: uint) -> Result<(), ()> {
		loop {
			let &(state, _) = match self.stack.last() {Some(x) => x, None => panic!()};
			let action = TABLE[state*NUM_SYMBOLS + tok_id];
			if action > NUM_RULES {
				return Ok(());
			}
			try!(self.reduce(action));
		}
	}
	fn reduce(&mut self, rule: uint) -> Result<(), ()> {
		match match rule {
			0 => Err(()),
			1 => {
				let sym = match self.stack.pop() { Some((_, "));
	try!(out.write_str(match grammar.rules[0].seq[0] {
		Sym(ref x) => x,
		_ => panic!()
	}.as_slice()));
	try!(out.write_str("(x))) => x, _ => panic!() };
				self.stack.push((1, Accept_(sym)));
				return Ok(());
			},"));
	let mut rule_id = 1u;
	for rule in grammar.rules.iter() {
		if rule_id == 1u {
			rule_id += 1;
			continue;
		}
		try!(format_args!(|args: &std::fmt::Arguments| out.write_fmt(args),
			"\n\t\t\t{} => {}", rule_id, '{'));
		let len = rule.seq.len();
		for i in range(0, len) {
			try!(format_args!(|args: &std::fmt::Arguments| out.write_fmt(args),
				"\n\t\t\t\tlet sym{} = match self.stack.pop() {} Some((_, {}(x))) => x, _ => panic!() {};",
				len - i - 1, '{', match rule.seq[len - i - 1] {
					Chr(_) => "Other".to_string(),
					Sym(ref s) => s.clone()
				}, '}'));
		}
		try!(format_args!(|args: &std::fmt::Arguments| out.write_fmt(args),
			"\n\t\t\t\tOk(({}, {}(try!(self.rule{}((", mapping[Sym(rule.nterm.deref().clone())], rule.nterm, rule_id));
		for i in range(0, len) {
			try!(format_args!(|args: &std::fmt::Arguments| out.write_fmt(args),
				"sym{}, ", i));
		}
		try!(format_args!(|args: &std::fmt::Arguments| out.write_fmt(args),
			"))))))\n\t\t\t{},", '}'));
		rule_id += 1;
	}
	try!(out.write_str("
			_ => panic!()
		} {
			Ok((id, x)) => {
				let &(st, _) = match self.stack.last() {Some(x) => x, None => panic!()};
				let goto = TABLE[st * NUM_SYMBOLS + id];
				println!(\"state {}, goto {}\", st, goto);
				self.stack.push((goto, x));
				Ok(())
			},
			Err(e) => Err(e)
		}
	}"));
	rule_id = 1;
	for rule in grammar.rules.iter() {
		if rule_id == 1u {
			rule_id += 1u;
			continue;
		}
		try!(format_args!(|args: &std::fmt::Arguments| out.write_fmt(args),
			"\n\tfn rule{}(&mut self, symbols: (", rule_id));
		let len = rule.seq.len();
		for i in range(0u, len) {
			match rule.seq[i] {
				Chr(_) => try!(out.write_str("char, ")),
				Sym(ref s) => {
					match grammar.nterms.find(&s.to_string()) {
						Some(x) => try!(x.type_.pretty_print_token(out, 1)),
						_ => try!(grammar.terms[s.to_string()].pretty_print_token(out, 1))
					}
					try!(out.write_str(", "));
				}
			}
		}
		try!(out.write_str(")) -> Result<"));
		try!(grammar.nterms[rule.nterm.deref().clone()].type_.pretty_print_token(out, 1));
		try!(out.write_str(", ()> "));
		try!(rule.code.pretty_print_token(out, 1));
		rule_id += 1;
	}
	try!(out.write_str("
	pub fn end_parse(mut self) -> Result<"));
	try!(grammar.nterms["Accept_".to_string()].type_.pretty_print_token(out, 1));
	try!(out.write_str(", ()> {
		loop {
			let state = match self.stack.last() {
				Some(&(_, Accept_(x))) => return Ok(x),
				Some(&(x, _)) => x,
				None => panic!()
			};
			let action = table[state*NUM_SYMBOLS];
			try!(self.reduce(action));
		}
	}
}"));
	Ok(())
}

fn main() {
	let path = Path::new("data/grammar");
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
	match tree.iter().pretty_print(&mut std::io::stdio::stdout(), 0, true) {
		Ok(_) => {},
		Err(e) => panic!("{}", e)
	}
	let grammar = parse_grammar(tree.as_slice());
	println!("{}", grammar);
	let nodes = create_nodes(&grammar);
	println!("{}", nodes);
	let (mapping, num_symbols) = assign_numbers(&nodes, &grammar);
	println!("mapping: {}", mapping);
	match write_parser(&Path::new("out.rs"), nodes, mapping, num_symbols, &grammar) {
		Ok(()) => {},
		Err(e) => panic!("could not write grammar: {}", e)
	}
}