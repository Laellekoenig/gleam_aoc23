import gleam/io
import simplifile
import gleam/list
import gleam/string
import gleam/int
import gleam/result

fn parse_input(in: String) -> List(List(Int)) {
  in
  |> string.split("\n")
  |> list.filter(fn(x) { !string.is_empty(x) })
  |> list.map(fn(x) {
    x
    |> string.split(" ")
    |> list.map(fn(y) {
      y
      |> int.parse
      |> result.unwrap(-1)
    })
  })
}

fn get_differences(lst: List(Int)) -> List(Int) {
  lst
  |> list.window_by_2
  |> list.map(fn(x) { x.1 - x.0 })
}

fn get_next(lst: List(Int)) -> Int {
  case list.all(lst, fn(x) { x == 0 }) {
    True -> 0
    False -> result.unwrap(list.last(lst), -1) + get_next(get_differences(lst))
  }
}

fn get_prev(lst: List(Int)) -> Int {
  case list.all(lst, fn(x) { x == 0 }) {
    True -> 0
    False -> result.unwrap(list.first(lst), -1) - get_prev(get_differences(lst))
  }
}

pub fn main() {
  let input = "input.txt"
  |> simplifile.read
  |> result.unwrap("")
  |> parse_input

  let p1 = input
  |> list.map(get_next)
  |> list.reduce(fn(acc, x) { acc + x })
  |> result.unwrap(-1)
  |> int.to_string

  io.println("-- Part 1 --")
  io.println("sum: " <> p1)

  let p2 = input
  |> list.map(get_prev)
  |> list.reduce(fn(acc, x) { acc + x })
  |> result.unwrap(-1)
  |> int.to_string

  io.println("-- Part 2 --")
  io.println("sum: " <> p2)
}
