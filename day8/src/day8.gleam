import gleam/io
import simplifile
import gleam/result
import gleam/string
import gleam/list
import gleam/dict
import gleam/int
import gleam_community/maths/arithmetics

pub type Step {
  Left
  Right
}

pub type Direction {
  Direction(left: String, right: String)
}

pub type Network {
  Network(steps: List(Step), map: dict.Dict(String, Direction))
}

fn parse_input(in: String) -> Network {
  let split = in
  |> string.split("\n")
  |> list.filter(fn(x) { !string.is_empty(x) })

  let steps = split
  |> list.first
  |> result.unwrap("")
  |> string.to_graphemes
  |> parse_steps

  let mappings = split
  |> list.drop(1)
  |> list.map(parse_mapping)

  Network(steps: steps, map: dict.from_list(mappings))
}

fn parse_steps(steps: List(String)) -> List(Step) {
  case steps {
    [] -> []
    [x, ..rest] -> {
      case x {
        "L" -> list.append([Left], parse_steps(rest))
        "R" -> list.append([Right], parse_steps(rest))
        _ -> panic as "Invalid step"
      }
    }
  }
}

fn parse_mapping(mapping: String) -> #(String, Direction) {
  let split = mapping
  |> string.split("=")
  |> list.map(string.trim)

  let key = split
  |> list.first
  |> result.unwrap("")

  let directions = split
  |> list.last
  |> result.unwrap("")
  |> string.drop_left(1)
  |> string.drop_right(1)
  |> string.split(", ")

  let left = directions
  |> list.first
  |> result.unwrap("")

  let right = directions
  |> list.last
  |> result.unwrap("")

  #(key, Direction(left, right))
}

fn count_steps(net: Network, start: String, is_end: fn(String) -> Bool) -> Int {
  count_steps_helper(net, is_end, start, 0)
}

fn count_steps_helper(net: Network, is_end: fn(String) -> Bool, curr: String, step: Int) -> Int {
  case is_end(curr) {
    True -> step
    False -> {
      let direction = dict.get(net.map, curr)
      |> result.unwrap(Direction("", ""))

      case list.at(net.steps, step % list.length(net.steps)) {
        Ok(Left) -> count_steps_helper(net, is_end, direction.left, step + 1)
        Ok(Right) -> count_steps_helper(net, is_end, direction.right, step + 1)
        _ -> panic as "Invalid state"
      }
    }
  }
}

fn ends_with(s: String, c: String) -> Bool {
  string.to_graphemes(s)
  |> list.last == Ok(c)
}

pub fn main() {
  let input = "input.txt"
  |> simplifile.read
  |> result.unwrap("")
  |> parse_input

  let p1 = input
  |> count_steps("AAA", fn(x) { x == "ZZZ"})
  |> int.to_string

  io.println("-- Part 1 --")
  io.println("Num steps: " <> p1)

  let p2 = input.map
  |> dict.to_list
  |> list.map(fn(x) { x.0 })
  |> list.filter(fn(x) { ends_with(x, "A") })
  |> list.map(count_steps(input, _, fn(x) { ends_with(x, "Z") }))
  |> list.reduce(fn(acc, x) { arithmetics.lcm(acc, x) })
  |> result.unwrap(-1)
  |> int.to_string

  io.println("-- Part 2 --")
  io.println("Num steps: " <> p2)
}
