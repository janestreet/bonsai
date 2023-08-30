open! Core
open! Import
open! Gen_js_api

(** An enum to represent the time granularity of the current graph.

    See https://github.com/danvk/dygraphs/blob/da2a028fc41e5573868358b3d9eda9826211d217/src/dygraph-tickers.js#L222 *)

type t =
  | MILLISECONDLY [@js 0]
  | TWO_MILLISECONDLY [@js 1]
  | FIVE_MILLISECONDLY [@js 2]
  | TEN_MILLISECONDLY [@js 3]
  | FIFTY_MILLISECONDLY [@js 4]
  | HUNDRED_MILLISECONDLY [@js 5]
  | FIVE_HUNDRED_MILLISECONDLY [@js 6]
  | SECONDLY [@js 7]
  | TWO_SECONDLY [@js 8]
  | FIVE_SECONDLY [@js 9]
  | TEN_SECONDLY [@js 10]
  | THIRTY_SECONDLY [@js 11]
  | MINUTELY [@js 12]
  | TWO_MINUTELY [@js 13]
  | FIVE_MINUTELY [@js 14]
  | TEN_MINUTELY [@js 15]
  | THIRTY_MINUTELY [@js 16]
  | HOURLY [@js 17]
  | TWO_HOURLY [@js 18]
  | SIX_HOURLY [@js 19]
  | DAILY [@js 20]
  | TWO_DAILY [@js 21]
  | WEEKLY [@js 22]
  | MONTHLY [@js 23]
  | QUARTERLY [@js 24]
  | BIANNUAL [@js 25]
  | ANNUAL [@js 26]
  | DECADAL [@js 27]
  | CENTENNIAL [@js 28]
  | NUM_GRANULARITIES [@js 29]
[@@js.enum] [@@deriving compare, sexp]

val t_of_js : Ojs.t -> t
val t_to_js : t -> Ojs.t
