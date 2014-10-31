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
				_ => fail!()
			}
		}
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
	/*grammar.terms.insert("EOF_".to_string(),Token {
			content: Tree(')', Vec::new()),
			ref_: CodeReference::internal()
		});*/
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
			Tree(..) => fail!()
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
								fail!() 
							} else {
								pos += 1;
							},
							_ => if !grammar.terms.insert(id.clone(), Token {
								content: Tree('}', Vec::new()),
								ref_: CodeReference::internal()
							}) {
								fail!();
							}
						}
					}
					_ => fail!()
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
					_ => fail!()
				};
				match tree[pos].content {
					Tree('}', ref t @ _) => {
						pos += 1;
						if startsymbol {
							match grammar.nterms.find_mut(&"Accept_".to_string()) {
								Some(x) => { x.type_ = type_.clone(); }
								None => fail!()
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
					_ => fail!()
				}
			},
			_ => fail!()
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
	let mut newentries: HashSet<uint> = HashSet::new();
	for &(r, pos) in state.iter() {
		let rule = &grammar.rules[r];
		if rule.seq.len() > pos {
			match rule.seq[pos] {
				Chr(_) => {},
				Sym(ref s) => {
					match grammar.nterms.find(s) {
						Some(ref sym) => {
							let (start, end) = sym.rules;
							for i in range(start,end) {
								newentries.insert(i);
							}
						}
						None => {
							match grammar.terms.find(s) {
								Some(_) => {},
								None => fail!("rule {} ({}) refers to unknown symbol {}",
										r, rule, s)
							}
						}
					}
				}
			}
		}
	}
	for rule in newentries.into_iter() {
		if !state.iter().any(|&(r, pos): &(uint, uint)| r == rule && pos == 0u) {
			state.push((rule, 0u));
		}
	}
}

fn derive_node(grammar: &Grammar, pos: uint, item: RuleItem, nodes: &mut Vec<Node>) {
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
	nodes.get_mut(pos).shifts.insert(item, idx);
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
		let node = nodes.get_mut(pos);
		println!("Node {}: {}", pos, node);
		for &(rule, p) in node.state.iter() {
			let r = &grammar.rules[rule];
			if r.seq.len() > p {
				shifts.insert(r.seq[pos].clone());
			} else {
				match node.reduce {
					None => { node.reduce = Some(rule); }
					Some(x) => fail!("Reduce-Reduce conflict in node {} ({}) between rule {} ({}) and {} ({})",
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
	let mut cur_id = 1u;
	let mut ids: HashMap<RuleItem, uint> = HashMap::new();
	ids.insert(Sym("$eof".to_string()), 0);
	for node in nodes.iter() {
		for (item, _) in node.shifts.iter() {
			if ids.insert(item.clone(), cur_id) {
				println!("{}: {}", item, cur_id);
				cur_id += 1;
			}
		}
	}
	// TODO: warn about unused symbols
	(ids, cur_id)
}

fn write_parser(filename: &Path, nodes: Vec<Node>, mapping: HashMap<RuleItem, uint>, num_symbols: uint, grammar: &Grammar)
		-> IoResult<()> {
	let mut file = try!(File::open_mode(filename, Truncate, Write));
	let out = &mut file;
	try!(grammar.prelude.iter().pretty_print(out, 0, false));
	try!(out.write_str("\nstatic table: &[uint] = &["));
	let num_rules = grammar.rules.len();
	for node in nodes.into_iter() {
		let mut line: Vec<uint> = Vec::with_capacity(num_symbols);
		line.grow(num_symbols, match node.reduce {
			Some(x) => x + 1,
			None => 0u
		});
		for (shift, target) in node.shifts.into_iter() {
			line[mapping[shift]]= target + num_rules;
		}
		println!("{}", line);
		try!(out.write_str("\n"));
		for num in line.into_iter() {
			try!(format_args!(|args: &std::fmt::Arguments| out.write_fmt(args), "{:8u},", num));
		}
	}
	try!(format_args!(|args: &std::fmt::Arguments| out.write_fmt(args),
		"\nstatic NUM_RULES = {}u;\nstatic NUM_SYMBOLS = {}u;\n", grammar.rules.len(), num_symbols));
	try!(out.write_str("];
pub struct Parser {
	stack: Vec<(uint, Token)>,
}

impl Parser {
	pub fn new() {
		Parser {stack: vec![(0u, Other('#')]}
	}
	fn get_token_id(tok: &Token) -> uint {
		match tok {"));
	for (item, id) in mapping.into_iter() {
		match item {
			Chr(c @ _) => {
				try!(out.write_str("\n\t\t\tOther"));
				try!(Token::internal(Tree(')',
					vec![Token::internal(Tok(Char(c)))])).pretty_print_token(out, 1));
				try!(format_args!(|args: &std::fmt::Arguments| out.write_fmt(args),
					" => {},", id));
			},
			Sym(ref s) if id != 0 => {
				try!(format_args!(|args: &std::fmt::Arguments| out.write_fmt(args),
					"\n\t\t\t{}(_) => {},", s, id));
			}
			_ => {}
		}
	}
	try!(out.write_str("\n\t\t\t_ => fail!(\"unknown token used in parser\")
		}
	}
	pub fn consume_token(&mut self, tok: Token) -> Result<(),()> {
		let &(mut state, _) = match self.stack.top();
		loop {
			let action = table[state*NUM_SYMBOLS + get_token_id(&tok)];
			if action > NUM_RULES {
				self.stack.push((action-NUM_RULES-1, tok));
				return Ok(());
			}
			try!(self.reduce(action));
			let &(st, _) = match self.stack.top();
			state = st;
		}
	}
	fn reduce(&mut self, rule: uint) {
		match rule {
			0 => Err(()),"));
	let mut rule_id = 0u;
	for rule in grammar.rules.iter() {
		try!(format_args!(|args: &std::fmt::Arguments| out.write_fmt(args),
			"\n\t\t\t{} => {}", rule_id + 1, '{'));
		let len = rule.seq.len();
		for i in range(0, len) {
			try!(format_args!(|args: &std::fmt::Arguments| out.write_fmt(args),
				"\n\t\t\t\tlet sym{} = match self.stack.pop() {} {}(x) => x, _ => fail!() {};",
				len - i - 1, '{', match rule.seq[len - i - 1] {
					Chr(_) => "Other".to_string(),
					Sym(ref s) => s.clone()
				}, '}'));
		}
		try!(format_args!(|args: &std::fmt::Arguments| out.write_fmt(args),
			"\n\t\t\t\ttry!(self.rule{}((", rule_id));
		for i in range(0, len) {
			try!(format_args!(|args: &std::fmt::Arguments| out.write_fmt(args),
				"sym{}, ", i));
		}
		try!(format_args!(|args: &std::fmt::Arguments| out.write_fmt(args),
			")))\n\t\t\t{},", '}'));
		rule_id += 1;
	}
	try!(out.write_str("
			_ => fail!()
		}
	}
}"));
	Ok(())
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
	match tree.iter().pretty_print(&mut std::io::stdio::stdout(), 0, true) {
		Ok(_) => {},
		Err(e) => fail!("{}", e)
	}
	let grammar = parse_grammar(tree.as_slice());
	println!("{}", grammar);
	let nodes = create_nodes(&grammar);
	println!("{}", nodes);
	let (mapping, num_symbols) = assign_numbers(&nodes, &grammar);
	match write_parser(&Path::new("out.rs"), nodes, mapping, num_symbols, &grammar) {
		Ok(()) => {},
		Err(e) => fail!("could not write grammar: {}", e)
	}
}