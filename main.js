/** a parser */
class Parser {
  /**
   * create a parser
   * @arg {Parser~handler} handler - the handler function for `parse_raw`
   * @example
   * let parser = new Parser(string => return {left: '', got: ['accept-all.']});
   * let out = parser.parse('hello, world!'); // ['accept-all']
   * console.assert(out.length === 1);
   * console.assert(out[0] === 'accept-all');
   */
  constructor(handler) {
    this.handler = handler;
  }
  /**
   * parse
   * @arg {String} string - the string to parse
   * @return {!String} res - parsing result
   */
  parse(string) {
    let res = this.parse_raw(string);
    return out?.res ?? null;
  }
  /**
   * parse without post-processing
   * @arg {String} string - the string to parse
   * @return {!Object} res - result
   * @return {String} res.left - parts of the string that are leftover
   * @return {*[]} res.got - parsing result
   */
  parse_raw(string) {
    return this.handler(string);
  }
  /**
   * parse this, otherwise parse something else
   * @arg {Parser} other - the aforementioned something else
   * @return {Parser} res - new parser
   */
  or(other) {
    return new Parser(string => {
      let mine = this.parse_raw(string);
      if (mine) return mine;
      return other.parse_raw(string);
    });
  }
  /**
   * parse this, then parse something else
   * @arg {Parser} other - the aforementioned something else
   * @return {Parser} res - new parser
   */
  then(other) {
    return new Parser(string => {
      let mine = this.parse_raw(string);
      if (mine) {
        let theirs = other.parse_raw(mine.left);
        if (theirs) {
          return {got: mine.got.concat(theirs.got), left: theirs.left};
        }
      }
      return null;
    })
  }
  /**
   * parse this, otherwise parse nothing
   * @return {Parser} res - new parser
   */
  maybe() {
    return new Parser(string => {
      let mine = this.parse_raw(string);
      if (mine) return mine;
      return {got: [null], left: string};
    });
  }
  /**
   * parse, then ignore the result
   * @return {Parser} res - new parser
   */
  ignore() {
    return new Parser(string => {
      let mine = this.parse_raw(string);
      if (mine)
        return {got: [], left: mine.left};
      return null;
    });
  }
  /**
   * repeatedly parse until no longer parsing successfully
   * @return {Parser} res - new parser
   */
  repeat() {
    return new Parser(string => {
      let result = {got: [], left: string};
      while (true) {
        let mine = this.parse_raw(result.left);
        if (mine) {
          result.left = mine.left;
          result.got = result.got.concat(mine.got);
        } else {
          return {got: [result.got], left: result.left};
        }
      }
    });
  }
  /**
   * process result of parser
   * @arg {Parser~processor} fn - processor
   * @return {Parser} res - new parser
   */
  process(fn) {
    return new Parser(string => {
      let mine = this.parse_raw(string);
      if (mine)
        return {got: [fn(...mine.got)], left: mine.left};
      return null;
    });
  }
  /**
   * parse, ignore the result, and use a different one
   * @arg {*} val - the aformentioned different result
   * @return {Parser} res - new parser
   */
  to(val) {
    return this.process((...args) => val);
  }
  /**
   * handler function for a parser.
   * @callback Parser~handler
   * @arg {String} string - the string to parse
   * @return {!Object} res - result
   * @return {String} res.left - parts of the string that are leftover
   * @return {*[]} res.got - parsing result
   */
  /**
   * processing function for a parser.
   * @callback Parser~processor
   * @arg {...*} orig - original result
   * @return {*} res - result
   */
}

/**
 * shorthand for chained `Parser.or`
 * @arg {...Parser} parsers - the parsers to chain
 * @return {Parser} parser - new parser
 */
function any(...parsers) {
  return parsers.reduce((x, y) => x.or(y));
}

/**
 * shorthand for chained `Parser.then`
 * @arg {...Parser} parsers - the parsers to chain
 * @return {Parser} parser - new parser
 */
function chain(...parsers) {
  return parsers.reduce((x, y) => x.then(y));
}

/**
 * seperate a parser with a delimiter
 * @arg {Parser} parser - the aformentioned parser
 * @arg {Parser} delim - the aformentioned delimiter, result is included in the final parser's result
 * @return {Parser} parser - new parser
 */
function seperate(parser, delim) {
  return parser.then(delim.then(parser).repeat()).process((first, ...rest) => [first].concat(rest[0]));
}

/**
 * parse a set string
 * @arg {strings} token - the aformentioned set string
 * @return {Parser} parser - new parser, with the result of `token`
 */
function just(token) {
  return new Parser(string => {
    if (string.startsWith(token))
      return {got: [token], left: string.slice(token.length)};
    return null;
  });
}

/**
 * match a regex
 * @arg {Regex} regex - the aformentioned regex
 * @return {Parser} parser - new parser, with the result of the regex match
 */
function match(regex) {
  let source = '^' + regex.source;
  let flags = regex.flags.replace('g', '')
                         .replace('m', '');
  let new_regex = new RegExp(source, flags);
  return new Parser(string=>{
    let possible_match = string.match(new_regex);
    if (possible_match)
      return {got: possible_match, left: string.slice(possible_match[0].length)};
    return null;
  });
}

/**
 * allows recursive parsing
 * @arg {recursive_parser} func - the parser to recursify
 * @return {Parser} res - new (recursive) parser
 */
function recursive(func) {
  let lazy_parser = new Parser(input => recursive(func).parse_raw(input));
  return func(lazy_parser);
}

/**
 * callback for `recursive`
 * @callback recursive_parser
 * @arg {Parser} self - same as `res`
 * @return {Parser} res - same as `self`
 */

/**
 * the lifesaver combinator. expression parsing.
 * @example
 * let expr = recursive({
 *   binopgen: (lhs, op, rhs) => op == '+' ? lhs + rhs : lhs - rhs,
 *   preopgen: (op, term) => -term,
 * }, expr =>
 *   operators(any(
 *     match(/-?\d+/).process(Number),
 *     chain(just('('), expr, just(')')),
 *   ),
 *     [{prefix: '-'}],
 *     [{infix: '+', assc: 'ltr'}, {infix: '-', assc: 'ltr'}]
 *   )
 * );
 * @arg {Object} gens - an object containing functions to generate the AST output
 * @arg {operators_binopgen} gens.binop - infix (binary) operator AST generator
 * @arg {operators_preopgen} gens.preop - prefix operator AST generator
 * @arg {operators_postopgen} gens.postop - postfix operator AST generator
 * @arg {Parser} term - parser for a single term in an expression
 * @arg {...Array} operators - operators
 * @arg {Object[]} operators[] - a single precedence of operators
 * @arg {{prefix: string}|{postfix: string}|{infix: string, assc: string}} operators[][] - a single operator, `assc` can either be `ltr` or `rtl`
 * @return {Parser} res - expression parser
 */
function operators(gens, term, ...operators) {
    let binopgen = gens.binop ?? (lhs, op, rhs) => ({type: 'binop', op, lhs, rhs});
    let preopgen = gens.preop ?? (op, term) => ({type: 'unop', fix: 'pre', op, term});
    let postopgen = gens.preop ?? (op, term) => ({type: 'unop', fix: 'post', op, term});
    function get_type(operator) {
        return operator.prefix ? 'prefix' :
               operator.postfix ? 'postfix' :
               operator.infix ? 'infix' :
               undefined;
    }
    return operators.reduce((term, ops) => {
        let types = new Set(ops.map(get_type));
        if (types.size !== 1)
            throw new Error('Multiple operators with the same precedence can\'t be of different types.');
        let type = get_type(ops[0]);
        if (!type)
            throw new Error(`Unknown operator type in: ${JSON.stringify(ops)}.`);
        let asscs = new Set(ops.map(e=>e.assc));
        if (asscs.size !== 1)
            throw new Error('Multiple operators with the same precedence can\'t be of different associativities.');
        let assc = ops[0].assc;
        if (assc !== 'ltr' && assc !== 'rtl')
            if (type === 'infix')
                throw new Error(`Unknown associativity: ${assc}.`);
        ops = ops.map(e=>e[type]);
        ops.sort((x, y) => y.length - x.length);
        let opparse = chain(match(/\s*/).ignore(), any(...ops.map(just)), match(/\s*/).ignore());
        switch (type) {
            case  'prefix': return opparse.repeat().then(term).process(
                (ops, term) => ops.reduceRight(preopgen, term)
            );
            case 'postfix': return term.then(opparse.repeat()).process(
                (term, ops) => ops.reduce(postopgen, term)
            );
            case  'infix':  return seperate(term, opparse).process(([term, ...rest]) => {
                if (assc === 'rtl')
                    [term, ...rest] = [term, ...rest].reverse()
                let pairs = rest.flatMap((_, i, a) => i % 2 ? [] : [a.slice(i++, ++i)]);
                return pairs.reduce(
                    (a, [op, b]) => binop(assc === 'ltr' ? a : b, op, assc === 'ltr' ? b : a)
                , term);
            });
        }
    }, term);
}

/**
 * callback for `operators` (`gens.binop`)
 * @callback operators_binopgen
 * @arg {*} lhs - left hand side
 * @arg {String} op - operator
 * @arg {*} rhs - right hand
 * @return {*} res - output
 */

/**
 * callback for `operators` (`gens.preop`)
 * @callback operators_preopgen
 * @arg {String} op - operator
 * @arg {*} term - term
 * @return {*} res - output
 */

/**
 * callback for `operators` (`gens.postop`)
 * @callback operators_postopgen
 * @arg {String} op - operator
 * @arg {*} term - term
 * @return {*} res - output
 */

module.exports = {Parser, chain, any, seperate, just, match, recursive, operators};