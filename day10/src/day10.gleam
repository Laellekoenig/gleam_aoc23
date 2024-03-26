import gleam/io
import simplifile
import gleam/string
import gleam/list
import gleam/int
import gleam/result
import gleam/dict.{type Dict}
import gleam/option.{type Option, None, Some}

pub type Pipe {
  Vertical
  Horizontal
  BendNE
  BendNW
  BendSW
  BendSE
  Start
}

pub type Coordinate {
  Coordinate(x: Int, y: Int)
}

pub type Map {
  Map(max_x: Int, max_y: Int, map: Dict(Coordinate, Pipe), start: Coordinate)
}

fn parse_tile(tile: String, x: Int, y: Int) -> Option(#(Coordinate, Pipe)) {
  case tile {
    "|" -> Some(#(Coordinate(x, y), Vertical))
    "-" -> Some(#(Coordinate(x, y), Horizontal))
    "L" -> Some(#(Coordinate(x, y), BendNE))
    "J" -> Some(#(Coordinate(x, y), BendNW))
    "7" -> Some(#(Coordinate(x, y), BendSW))
    "F" -> Some(#(Coordinate(x, y), BendSE))
    "S" -> Some(#(Coordinate(x, y), Start))
    "." -> None
    _ -> panic as "Invalid tile"
  }
}

fn parse_line(line: String, y: Int) -> List(#(Coordinate, Pipe)) {
  line
  |> string.to_graphemes
  |> list.index_map(fn(tile, x) {
    parse_tile(tile, x, y)
  })
  |> list.filter(fn(x) {
    case x {
      Some(_) -> True
      None -> False
    }
  })
  |> list.map(fn(x) {
    case x {
      Some(x) -> x
      _ -> panic as "Should not get here"
    }
  })
}

fn parse_lines(lines: List(String), y: Int, res: List(#(Coordinate, Pipe))) -> List(#(Coordinate, Pipe)) {
  case lines {
    [] -> res
    [x, ..rest] -> {
      let parsed_line = parse_line(x, y)
      parse_lines(rest, y + 1, list.append(res, parsed_line))
    }
  }
}

fn parse_input(in: String) -> Map {
  let split = in
  |> string.split("\n")
  |> list.filter(fn(x) { !string.is_empty(x) })

  let map = split
  |> parse_lines(0, [])
  |> dict.from_list

  let start = dict.to_list(map)
  |> list.filter(fn(x) {
    case x.1 {
      Start -> True
      _ -> False
    }
  })
  |> list.first
  |> result.unwrap(#(Coordinate(-1, -1), Start))

  let max_y = list.length(split) - 1
  let max_x = string.length(result.unwrap(list.first(split), "")) - 1

  Map(max_x: max_x, max_y: max_y, map: map, start: start.0)
}

fn get_path_length_helper(map: Dict(Coordinate, Pipe), curr: Coordinate, len: Int, left_start: Bool) -> Int {

}

fn get_path_length(map: Map) -> Int {
  get_path_length_helper(map.map, map.start, 0, False)
}

pub fn main() {
  let input = "test.txt"
  |> simplifile.read
  |> result.unwrap("")
  |> parse_input

  let p1 = input
  |> get_path_length
  |> io.debug
}
