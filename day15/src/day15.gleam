import gleam/io
import simplifile
import gleam/string
import gleam/list
import gleam/result
import gleam/int
import gleam/dict.{type Dict}

pub type Lens {
  Lens(label: String, focal_length: Int)
}

pub type Instruction {
  RemoveLens(label: String, hash: Int)
  InsertLens(lens: Lens, hash: Int)
}

fn parse(in: String) -> List(String) {
  in
  |> string.split(",")
  |> list.map(string.to_graphemes)
  |> list.map(list.filter(_, fn(x) { x != "\n" }))
  |> list.map(fn(x) { string.join(x, "")})
}

fn hash(in: String) -> Int {
  in
  |> string.to_utf_codepoints
  |> list.map(string.utf_codepoint_to_int)
  |> list.fold(0, fn(acc, x) {
    { { acc + x } * 17 } % 256
  })
}

fn parse_instruction(s: String) -> Instruction {
  case string.contains(s, "=") {
    True -> {
      let split = s
      |> string.split("=")

      let label = split
      |> list.first
      |> result.unwrap("")

      let focal_length = split
      |> list.last
      |> result.unwrap("")
      |> int.parse
      |> result.unwrap(-1)

      InsertLens(Lens(label, focal_length), hash(label))
    }
    False -> {
      let label = s
      |> string.split("-")
      |> list.first
      |> result.unwrap("")

      RemoveLens(label, hash(label))
    }
  }
}

fn get_tuples(res: List(#(Int, List(Lens)))) -> List(#(Int, List(Lens))) {
  case res {
    [] -> get_tuples([#(0, [])])
    [x, ..rest] if x.0 < 255 -> get_tuples(list.append([#(x.0 + 1, []), x], rest))
    _ -> res
  }
}

fn get_hashmap() -> Dict(Int, List(Lens)) {
  []
  |> get_tuples
  |> dict.from_list
}

fn apply_instruction(i: Instruction, map: Dict(Int, List(Lens))) -> Dict(Int, List(Lens)) {
  case i {
    RemoveLens(label, hash) -> {
      let new_box = dict.get(map, hash)
      |> result.unwrap([])
      |> list.filter(fn(x) { x.label != label })

      map
      |> dict.insert(hash, new_box)
    }
    InsertLens(lens, hash) -> {
      let box = dict.get(map, hash)
      |> result.unwrap([])

      let new_box = case list.find(box, fn(l) { l.label == lens.label }) {
        Ok(_) -> list.map(box, fn(x) {
          case x.label == lens.label {
            True -> lens
            False -> x
          }
        })
        Error(_) -> list.append(box, [lens])
      }

      map
      |> dict.insert(hash, new_box)
    }
  }
}

fn apply_instructions(instructions: List(Instruction), map: Dict(Int, List(Lens))) -> Dict(Int, List(Lens)) {
  case instructions {
    [] -> map
    [x, ..rest] -> {
      apply_instructions(rest, apply_instruction(x, map))
    }
  }
}

fn get_box_power(box: #(Int, List(Lens))) -> Int {
  let #(box_i, lens_lst) = box

  lens_lst
  |> list.index_fold(0, fn(acc, x, i) {
    acc + { { box_i + 1 } * { i + 1 } * x.focal_length }
  })
}

fn get_focusing_power(map: Dict(Int, List(Lens))) -> Int {
  map
  |> dict.to_list
  |> list.map(get_box_power)
  |> list.reduce(fn(acc, x) { acc + x })
  |> result.unwrap(-1)
}

pub fn main() {
  let input = "input.txt"
  |> simplifile.read
  |> result.unwrap("")
  |> parse

  let p1 = input
  |> list.map(hash)
  |> list.reduce(int.add)
  |> result.unwrap(-1)
  |> int.to_string

  io.println("-- Part 1 --")
  io.println("Sum: " <> p1)

  let map = get_hashmap()

  let p2 = input
  |> list.map(parse_instruction)
  |> apply_instructions(map)
  |> get_focusing_power
  |> int.to_string

  io.println("-- Part 2 --")
  io.println("Focusing power: " <> p2)
}
