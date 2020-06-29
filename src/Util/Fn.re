/* Basic functions missing from standard library. TODO: just import something */

/* Because Relude only implements scanl'.
   This implementation is copied from Haskell: https://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.List.html#scanl */
let rec scanl: (('b, 'a) => 'b, 'b, list('a)) => list('b) =
  (f, q, ls) => {
    List.cons(
      q,
      switch (ls) {
      | [] => []
      | [x, ...xs] => scanl(f, f(q, x), xs)
      },
    );
  };

/* https://2ality.com/2018/01/lists-arrays-reasonml.html */
/**
* Compute a list of integers starting with `start`,
* up to and excluding `end_`.
*/
let rec range = (start: int, end_: int) =>
  if (start >= end_) {
    [];
  } else {
    [start, ...range(start + 1, end_)];
  };

let rec product = (l1, l2) =>
  switch (l1, l2) {
  | ([], _)
  | (_, []) => []
  | ([h1, ...t1], [h2, ...t2]) => [(h1, h2), ...product([h1], t2)] @ product(t1, l2)
  };

let rec repeat = (n, a) =>
  if (n == 0) {
    [];
  } else {
    [a, ...repeat(n - 1, a)];
  };

/* https://stackoverflow.com/questions/31279920/finding-an-item-in-a-list-and-returning-its-index-ocaml */
let rec find = (x, lst) =>
  switch (lst) {
  | [] => raise(failwith("Not Found"))
  | [h, ...t] =>
    if (x == h) {
      0;
    } else {
      1 + find(x, t);
    }
  };

/* https://stackoverflow.com/a/53914427 */
let rec mapPairs = (f: ('a, 'a) => 'b, xs: list('a)): list('b) =>
  switch (xs) {
  | [x, y, ...xs] => [f(x, y), ...mapPairs(f, [y, ...xs])]
  | _ => []
  };