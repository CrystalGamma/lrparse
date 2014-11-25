Integrating lrparse parsers
===========================

lrparse generates a Rust file with the following exported items:

* enum Token: this enum contains variants for every symbol in the grammar, as well as a variant Other(char) used for the character terminal symbols.
* enum Error: this enum contains all Error codes described in the grammar
* struct Parser: the parser generated for the grammar. See below.

Parser public methods
---------------------

* new(pos: CodeReference) -> Parser: creates a new parser, using the given CodeReference as start of the document
* consume_token(&mut self, tok: Token, pos: CodeReference) -> Result<(), Error>: inputs a token into the parsing process.
	Returns Ok(()) if no Error occurs, Err(Error::SyntaxError(pos)) with pos as the CodeReference
	for the beginning of the offending token if a formal parsing error occurs,
	and Err(err) with err for the given error code if a rule returned with an error code.
* end_parse(self, pos: CodeReference) -> Result<type, Error>: tries to end the parse,
	returns Err(Error::SyntaxError(pos)) with pos as the argument of end_parse if the parser is not in an accepting state,
	Err(err) with err for the given error code if a rule returned with an error code.
	or Ok(val) with val as the value for the start symbol if the parse was successful