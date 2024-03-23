import gleam/io
import simplifile
import gleam/result
import gleam/list
import gleam/string
import gleam/int

fn parse_num_list(s: String) -> List(Int) {
  s
  |> string.split(" ")
  |> list.map(fn(x) { result.unwrap(int.parse(x), -1) })
}

fn parse_seeds(s: String) -> List(Int) {
  s
  |> string.split(": ")
  |> list.last
  |> result.unwrap("")
  |> parse_num_list
}

pub type MapLine {
  MapLine(dest: Int, src: Int, len: Int)
}

fn list_to_map(lst: List(Int)) -> MapLine {
  case lst {
    [x, y, z] -> MapLine(x, y, z)
    _ -> MapLine(-1, -1, -1)
  }
}

fn parse_map(s: String) -> List(MapLine) {
  s
  |> string.split(":\n")
  |> list.last
  |> result.unwrap("")
  |> string.split("\n")
  |> list.filter(fn(x) { !string.is_empty(x) })
  |> list.map(parse_num_list)
  |> list.map(list_to_map)
}

fn apply_map(seed: Int, map: List(MapLine)) -> Int {
  let map_lines = map
  |> list.filter(fn(map_line) {
    map_line.src <= seed && map_line.src + map_line.len > seed
  })

  case map_lines {
    [] -> seed
    [map_line] -> map_line.dest + { seed - map_line.src }
    _ -> panic as "More than one map_line mapped"
  }
}

fn get_location(seed: Int, maps: List(List(MapLine))) -> Int {
  case maps {
    [] -> seed
    [map, ..rest] -> {
      let seed = seed
      |> apply_map(map)

      get_location(seed, rest)
    }
  }
}

fn get_min(lst: List(Int)) -> Int {
  lst
  |> list.reduce(fn(acc, x) {
    case acc > x {
      True -> x
      False -> acc
    }
  })
  |> result.unwrap(-1)
}

pub type Range {
  Range(src: Int, len: Int)
}

fn get_seed_ranges(seeds: List(Int)) -> List(Range) {
  get_seed_ranges_helper(seeds, [])
}

fn get_seed_ranges_helper(seeds: List(Int), res: List(Range)) -> List(Range) {
  case seeds {
    [start, len, ..rest] -> {
      let range = Range(start, len)
      get_seed_ranges_helper(rest, list.append(res, [range]))
    }
    [] -> res
    _ -> panic as "Not an even number of seeds"
  }
}

fn map_ranges(ranges: List(Range), map: List(MapLine)) -> List(Range) {
  map_ranges_helper(ranges, map, [])
}

fn map_ranges_helper(ranges: List(Range), map: List(MapLine), res: List(Range)) -> List(Range) {
  case ranges {
    [] -> res
    [r, ..rest] -> {
      let new_ranges = map_range(r, map)
      map_ranges_helper(rest, map, list.append(res, new_ranges))
    }
  }
}

fn map_range(range: Range, map: List(MapLine)) -> List(Range) {
  let candidates = map
  |> list.filter(fn(x) { x.src < range.src + range.len && x.src + x.len > range.src})

  case candidates {
    [] -> [range]
    [m] -> {
      let larger = m.src < range.src && m.src + m.len > range.src + range.len
      let equal = m.src == range.src && m.len == range.len
      let contained = m.src > range.src && m.src + m.len < range.src + range.len
      let clip_l = m.src < range.src
      let clip_r = m.src + m.len > range.src + range.len

      case larger, equal, contained, clip_l, clip_r {
        // larger
        True, _, _, _, _ -> [Range(m.dest + { range.src - m.src }, range.len)]

        // equal
        _, True, _, _, _ -> [Range(m.dest, m.len)]

        // contained
        _, _, True, _ ,_ -> {
          let r0 = Range(range.src, m.src - range.src)
          let r1 = Range(m.dest, m.len)
          let r2 = Range(m.src + m.len, range.len - m.len - { m.src - range.src })
          [r0, r1, r2]
        }

        // clip_l
        _, _, _, True, _ -> {
          let r0 = Range(m.dest + { range.src - m.src }, m.src + m.len - range.src)
          let r1 = Range(m.src + m.len, range.len - { m.src + m.len - range.src })
          [r0, r1]
        }

        // clip_r
        _, _, _, _, True -> {
          let r0 = Range(range.src, m.src - range.src)
          let r1 = Range(m.dest, range.src + range.len - m.src)
          [r0, r1]
        }

        _, _, _, _, _ -> panic as "Should not get here"
      }
    }
    _ -> panic as "TODO: more than one range hit"
  }
}

fn pt_2(seeds: List(Range), maps: List(List(MapLine))) -> List(Range) {
  case maps {
    [] -> seeds
    [m, ..rest] -> {
      let new_seeds = map_ranges(seeds, m)
      |> io.debug
      pt_2(new_seeds, rest)
    }
  }
}

pub fn main() {
  let input = "input.txt"
  let input = "test.txt"
  |> simplifile.read
  |> result.unwrap("")
  |> string.split("\n\n")

  let #(seeds, maps) = input
  |> list.split(1)

  let seeds = seeds
  |> list.first
  |> result.unwrap("")
  |> parse_seeds

  let maps = maps
  |> list.map(parse_map)

  let closest = seeds
  |> list.map(get_location(_, maps))
  |> get_min
  |> int.to_string

  io.println("-- Part 1 --")
  io.println("Closest: " <> closest)

  let final_ranges = seeds
  |> get_seed_ranges
  |> pt_2(maps)
  |> io.debug

  io.println("-- Part 2 --")
  //io.println("Closest from seed ranges: " <> closest_range)
}
