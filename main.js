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
  constructor(handler, ...args) {
    this.handler = handler;
    this.args = args;
  }
  /**
   * parse
   * @arg {String} string - the string to parse
   * @return {!String} res - parsing result
   */
  parse(string) {
    let out = this.parse_raw(string);
    return out?.got ?? null;
  }
  /**
   * parse without post-processing
   * @arg {String} string - the string to parse
   * @return {!Object} res - result
   * @return {String} res.left - parts of the string that are leftover
   * @return {*[]} res.got - parsing result
   */
  parse_raw(string) {
    return this.handler(...this.args, string);
  }
  /**
   * parse this, otherwise parse something else
   * @arg {Parser} other - the aforementioned something else
   * @return {Parser} res - new parser
   */
  or(other) {
    return new Parser((that, other, string) => {
      let mine = that.parse_raw(string);
      if (mine) return mine;
      return other.parse_raw(string);
    }, this, other);
  }
  /**
   * parse this, then parse something else
   * @arg {Parser} other - the aforementioned something else
   * @return {Parser} res - new parser
   */
  then(other) {
    return new Parser((that, other, string) => {
      let mine = that.parse_raw(string);
      if (mine) {
        let theirs = other.parse_raw(mine.left);
        if (theirs) {
          return {got: mine.got.concat(theirs.got), left: theirs.left};
        }
      }
      return null;
    }, this, other)
  }
  /**
   * parse this, otherwise parse nothing
   * @return {Parser} res - new parser
   */
  maybe() {
    return new Parser((that, string) => {
      let mine = that.parse_raw(string);
      if (mine) return mine;
      return {got: [null], left: string};
    }, this);
  }
  /**
   * parse, then ignore the result
   * @return {Parser} res - new parser
   */
  ignore() {
    return new Parser((that, string) => {
      let mine = that.parse_raw(string);
      if (mine)
        return {got: [], left: mine.left};
      return null;
    }, this);
  }
  /**
   * repeatedly parse until no longer parsing successfully
   * @return {Parser} res - new parser
   */
  repeat() {
    return new Parser((that, string) => {
      let result = {got: [], left: string};
      while (true) {
        let mine = that.parse_raw(result.left);
        if (mine) {
          result.left = mine.left;
          result.got = result.got.concat(mine.got);
        } else {
          return {got: [result.got], left: result.left};
        }
      }
    }, this);
  }
  /**
   * process result of parser
   * @arg {Parser~processor} fn - processor
   * @return {Parser} res - new parser
   */
  process(fn, ...extra_args) {
    return new Parser((that, fn, extra_args, string) => {
      let mine = that.parse_raw(string);
      if (mine)
        return {got: [fn(...extra_args, ...mine.got)], left: mine.left};
      return null;
    }, this, fn, extra_args);
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
  /**
   * seperate a parser with a delimiter
   * @arg {Parser} delim - the aformentioned delimiter, result is included in the final parser's result
   * @return {Parser} parser - new parser
   */
  seperate(delim) {
    return this.then(delim.then(this).repeat()).process((first, ...rest) => [first].concat(rest[0]));
  }
  /**
   * parse, but don't eat any characters if parsing successfully
   * @return {Parser} res - new parser
   */
  lookahead() {
    return new Parser((that, string) => {
      let mine = that.parse_raw(string);
      if (mine)
        return {got: mine.got, left: string};
      return null;
    }, this);
  }
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
 * parse a set string
 * @arg {String} token - the aformentioned set string
 * @return {Parser} parser - new parser, with the result of `token`
 */
function just(token) {
  return new Parser((token, string) => {
    if (string.startsWith(token))
      return {got: [token], left: string.slice(token.length)};
    return null;
  }, token);
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
  return new Parser((regex, string) => {
    let possible_match = string.match(regex);
    if (possible_match)
      return {got: possible_match, left: string.slice(possible_match[0].length)};
    return null;
  }, new_regex);
}

/**
 * allows recursive parsing
 * @arg {recursive_parser} func - the parser to recursify
 * @return {Parser} res - new (recursive) parser
 */
function recursive(func) {
  let lazy_parser = new Parser(
    input => recursive(func).parse_raw(input)
  );
  return func(lazy_parser);
}

/**
 * callback for `recursive`
 * @callback recursive_parser
 * @arg {Parser} self - same as `res`
 * @return {Parser} res - same as `self`
 */

/**
 * more efficient way of writing `any(just(), just(), ...)`
 * @arg {...String} possibilities - the strings
 * @return {Parser} parser - new parser
 */
function narrow_down(...ps) {
  return new Parser((ps, string) => {
     let i;
     let chars = string.length;
     for (i = 0; i < chars; i++) {
      let new_ps = ps.filter(p => p[i] == string[i]);
      if (new_ps.length == 1)
        ps = new_ps;
      if (new_ps.length <= 1)
        break;
    }
    if (ps.length > 1)
      ps = ps.filter(p => p.length == i);
    if (ps.length == 0) return null;
    return {got: ps, left: string.slice(ps[0].length)};
  }, ps);
}

/**
 * the lifesaver combinator. expression parsing.
 * @example
 * let {any, operators, recursive, common: {int, parenthesized}} = require('@otesunki/numb');
 * 
 * let expr = recursive(expr =>
 *   operators({
 *     infopgen: (lhs, op, rhs) => op == '+' ? lhs + rhs : lhs - rhs,
 *     preopgen: (op, term) => -term,
 *   }, any(
 *     int,
 *     parenthesized(expr),
 *   ),
 *     [{prefix: '-'}],
 *     [{infix: '+', assc: 'ltr'}, {infix: '-', assc: 'ltr'}]
 *   )
 * );
 * @arg {Object} gens - an object containing functions to generate the AST output
 * @arg {operators_infopgen} gens.infop - infix (binary) operator AST generator
 * @arg {operators_preopgen} gens.preop - prefix operator AST generator
 * @arg {operators_postopgen} gens.postop - postfix operator AST generator
 * @arg {Parser} term - parser for a single term in an expression
 * @arg {...Array} operators - operators
 * @arg {Object[]} operators[] - a single precedence of operators
 * @arg {{prefix: string}|{postfix: string}|{infix: string, assc: string}} operators[][] - a single operator, `assc` can either be `ltr` or `rtl`
 * @return {Parser} res - expression parser
 */
function operators(gens, term, ...operators) {
    let infopgen = gens.infop ?? ((lhs, op, rhs) => ({type: 'binop', op, lhs, rhs}));
    let preopgen = gens.preop ?? ((term, op) => ({type: 'unop', fix: 'pre', op, term}));
    let postopgen = gens.postop ?? ((term, op) => ({type: 'unop', fix: 'post', op, term}));
    function get_type(operator) {
        return operator.prefix ? 'prefix' :
               operator.postfix ? 'postfix' :
               operator.infix ? 'infix' :
               undefined;
    }
    operators.reverse();
    let binding_powers = operators.flatMap((prec, i) => {
        let types = new Set(prec.map(get_type));
        if (types.size !== 1)
            throw new Error('Multiple operators with the same precedence can\'t be of different types.');
        let type = get_type(prec[0]);
        if (!type)
            throw new Error(`Unknown operator type in: ${JSON.stringify(ops)}.`)
        let asscs = new Set(prec.map(e=>e.assc));
        if (asscs.size !== 1)
            throw new Error('Multiple operators with the same precedence can\'t be of different associativities.');
        let assc = prec[0].assc;
        if (assc !== 'ltr' && assc !== 'rtl')
            if (type === 'infix')
                throw new Error(`Unknown associativity: ${assc}.`)
        let left = -1;
        let rght = -1;
        if (type == 'infix') {
            left = assc == 'rtl' ? i*2 : i*2+1;
            rght = assc == 'ltr' ? i*2 : i*2+1;
        } else if (type == 'prefix') {
            rght = i*2;
        } else if (type == 'postfix') {
            left = i*2;
        }
        return prec.map(e=>({[e[type]]: [left, rght]}));
    });
    let infix_binding = binding_powers.filter(obj => {
        let [left, rght] = Object.values(obj)[0];
        return left !== -1 && rght !== -1;
    }).reduce((x, y) => Object.assign(x, y), {});
    let prefix_binding = binding_powers.filter(obj => {
        let [left, rght] = Object.values(obj)[0];
        return left === -1;
    }).reduce((x, y) => Object.assign(x, y), {});
    let postfix_binding = binding_powers.filter(obj => {
        let [left, rght] = Object.values(obj)[0];
        return rght === -1;
    }).reduce((x, y) => Object.assign(x, y), {});
    return new Parser((infopgen, preopgen, postopgen, term, infix_binding, prefix_binding, postfix_binding, string) => {
        let prefixes = Object.keys(prefix_binding);
        let infixes = Object.keys(infix_binding);
        let postfixes = Object.keys(postfix_binding);
        function expr(string, min_bp) {
            let preop = narrow_down(...prefixes).parse_raw(string);
            if (preop !== null)
                string = preop.left;
            let lhs = term.parse_raw(string);
            if (lhs === null) return null;
            string = lhs.left;
            lhs = lhs.got[0];
            if (preop !== null)
                lhs = preopgen(lhs, preop.got[0])
            outer: for (;;) {
                let parsed = chain(
                  match(/\s*/).ignore(),
                  narrow_down(...postfixes),
                  match(/\s*/).ignore(),
                ).parse_raw(string);
                if (parsed !== null) {
                    let new_str = parsed.left;
                    let [op] = parsed.got;
                    let [l_bp, r_bp] = postfix_binding[op];
                    if (l_bp < min_bp) break;
                    string = new_str;
                    lhs = postopgen(lhs, op);
                }
                parsed = chain(
                  match(/\s*/).ignore(),
                  narrow_down(...infixes),
                  match(/\s*/).ignore(),
                ).parse_raw(string);
                if (parsed !== null) {
                    let new_str = parsed.left;
                    let [op] = parsed.got;
                    let [l_bp, r_bp] = infix_binding[op];
                    if (l_bp < min_bp) break;
                    let rhs = expr(new_str, r_bp);
                    if (rhs === null) break outer;
                    string = rhs.left;
                    rhs = rhs.got[0];
                    lhs = infopgen(lhs, op, rhs);
                }
                break;
            }
            return {got: [lhs], left: string};
        }
        return expr(string, 0);
    }, infopgen, preopgen, postopgen, term, infix_binding, prefix_binding, postfix_binding);
}

/**
 * callback for `operators` (`gens.infop`)
 * @callback operators_infopgen
 * @arg {*} lhs - left hand side
 * @arg {String} op - operator
 * @arg {*} rhs - right hand
 * @return {*} res - output
 */

/**
 * callback for `operators` (`gens.preop`)
 * @callback operators_preopgen
 * @arg {*} term - term
 * @arg {String} op - operator
 * @return {*} res - output
 */

/**
 * callback for `operators` (`gens.postop`)
 * @callback operators_postopgen
 * @arg {*} term - term
 * @arg {String} op - operator
 * @return {*} res - output
 */

let eof = new Parser(string => {
  if (string.length == 0)
    return {got: [], left: string};
  return null;
});

let nat = match(/\d+/).process(parseInt);
let int = match(/-?\d+/).process(parseInt);
let float = match(/-?(\d(\.\d*)|\.\d+|Infinity)|NaN/).process(parseFloat);
let parenthesized = parser => chain(just('(').ignore(), parser, just(')').ignore());

module.exports = {
  Parser,
  chain,
  any,
  just,
  match,
  recursive,
  operators,
  eof,
  common: {
    nat,
    int,
    float,
    parenthesized,
  }
};