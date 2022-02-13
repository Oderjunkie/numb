# numb
a PEG combinatorial parser

## install
`npm i @otesunki/numb`

## example
```js
let {any, chain, just, match, operators, recursive} = require('@otesunki/numb');

let expr = recursive(expr =>
  operators({
    infop: (lhs, op, rhs) => op == '+' ? lhs + rhs : lhs - rhs,
    preop: (term, op) => -term,
  }, any(
    match(/\d+/).process(Number),
    chain(just('(').ignore(), expr, just(')').ignore()),
  ),
    [{prefix: '-'}],
    [{infix: '+', assc: 'ltr'}, {infix: '-', assc: 'ltr'}]
  )
);

console.log(expr.parse_raw('(1 + 2) * -3'));
```