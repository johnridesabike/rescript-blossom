/*******************************************************************************
  MIT License

  Copyright (c) 2020 John Jackson

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 ******************************************************************************/

type internalCmp('vertex, 'identity);
type internalEdgeCmp('v, 'identity, 'vertexIdentity);

module type Comparable = {
  type t;
  type identity;
  type edgeIdentity;
  let cmp: internalCmp(t, identity);
  let edgeCmp: internalEdgeCmp(t, edgeIdentity, identity);
  module BeltCmp:
    Belt.Id.Comparable with type t = t and type identity = identity;
};

/**
 * A module of functions with a unique identity type. It is analogous to the
 * type `Belt.Id.comparable`. Create one with the `comparable` function or the
 * `MakeComparable` functor. Or create one with an existing `Belt.Id.comparable`
 * module with `unsafeComparableFromBelt`.
 *
 * ```
 * module StringCmp = (val comparable(compare: (string, string) => int));
 *
 * module IntCmp = MakeComparable({
 *   type t = int;
 *   let cmp: (t, t) => int = compare;
 * });
 * ```
 */
type comparable('vertex, 'identity) = (module Comparable with
                                          type identity = 'identity and
                                          type t = 'vertex);

/**
 * Represents whether or not the algorithm should *only* accept maximum-
 * cardinality solutions.
 */
type cardinality = [ | `Max | `NotMax];

/**
 * A bi-directional, read-only mapping of each vertex to its mate vertex.
 */
type t('vertex, 'identity);

/**
 * Compute a maximum-weighted matching in the general undirected weighted graph.
 *
 * Accepts a list of tuples `(i, j, w)`, each describing an undirected edge
 * between vertex `i` and vertex `j` with weight `w`. There is at most one edge
 * between any two vertices, and no vertex has an edge to itself. Duplicate
 * edges are ignored.
 *
 * `id` accepts a first-class module created by `comparable` or
 * `MakeComparable`.
 *
 * If `cardinality` is set to `` `Max ``, only maximum-cardinality matchings are
 * considered as solutions.
 *
 * This function takes time O(n ** 3).
 */
let make:
  (
    ~cardinality: cardinality=?,
    list(('vertex, 'vertex, float)),
    ~id: comparable('vertex, 'identity)
  ) =>
  t('vertex, 'identity);

/**
 * Returns `Some(vertex)` for a mated vertex, or `None` if none exists.
 */
let get: (t('vertex, 'identity), 'vertex) => option('vertex);

/**
 * Reduces over the pairs of vertex mates. Each pair is used twice, once in each
 * order.
 * Takes an uncurried `f` function.
 */
let reduceU:
  (
    t('vertex, 'identity),
    ~init: 'acc,
    ~f: (. 'acc, 'vertex, 'vertex) => 'acc
  ) =>
  'acc;

/**
 * Reduces over the pairs of vertex mates. Each pair is used twice, once in each
 * order.
 */
let reduce:
  (
    t('vertex, 'identity),
    ~init: 'acc,
    ~f: ('acc, 'vertex, 'vertex) => 'acc
  ) =>
  'acc;

/**
 * Iterates over the pairs of vertex mates. Each pair is used twice, once in
 * order.
 * Takes an uncurried `f` function.
 */
let forEachU:
  (t('vertex, 'identity), ~f: (. 'vertex, 'vertex) => unit) => unit;

/**
 * Iterates over the pairs of vertex mates. Each pair is used twice, once in
 * each order.
 */
let forEach: (t('vertex, 'identity), ~f: ('vertex, 'vertex) => unit) => unit;

/**
 * Returns a `Belt.Map.t` where each key is a vertex and each value is its mate.
 */
let toMap: t('vertex, 'identity) => Belt.Map.t('vertex, 'vertex, 'identity);

/**
 * Returns a list of tuples for each pair of vertex mates. Each pair is used
 * twice, once in each order.
 */
let toList: t('vertex, 'identity) => list(('vertex, 'vertex));

/**
 * Returns `true` if there are no mates, `false` otherwise.
 */
let isEmpty: t('vertex, 'identity) => bool;

/**
 * Returns `true` if the vertex has a mate, `false` otherwise.
 */
let has: (t('vertex, 'identity), 'vertex) => bool;

module Int: {
  module Cmp: Comparable with type t = int;
  type nonrec t = t(int, Cmp.identity);
  let make: (~cardinality: cardinality=?, list((int, int, float))) => t;
};

module String: {
  module Cmp: Comparable with type t = string;
  type nonrec t = t(string, Cmp.identity);
  let make:
    (~cardinality: cardinality=?, list((string, string, float))) => t;
};

/**
 * This is unsafe because we can't guarantee the `cmp` value will be correct.
 */
let unsafeComparableFromBelt:
  (
    ~id: Belt.Id.comparable('vertex, 'identity),
    ~cmp: ('vertex, 'vertex) => int
  ) =>
  comparable('vertex, 'identity);

/**
 * This is unsafe because we can't guarantee the `cmp` value will be correct.
 */
let unsafeComparableFromBeltU:
  (
    ~id: Belt.Id.comparable('vertex, 'identity),
    ~cmp: (. 'vertex, 'vertex) => int
  ) =>
  comparable('vertex, 'identity);

module MakeComparable:
  (M: {
     type t;
     let cmp: (t, t) => int;
   }) => Comparable with type t = M.t;

module MakeComparableU:
  (M: {
     type t;
     let cmp: (. t, t) => int;
   }) => Comparable with type t = M.t;

let comparable:
  (('vertex, 'vertex) => int) => (module Comparable with type t = 'vertex);

let comparableU:
  ((. 'vertex, 'vertex) => int) => (module Comparable with type t = 'vertex);

let comparableToBelt:
  comparable('vertex, 'identity) => Belt.Id.comparable('vertex, 'identity);
