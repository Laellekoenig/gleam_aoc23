import gleam/io
import simplifile
import gleam/result
import gleam/string
import gleam/list
import gleam/int

pub type Symbol {
  Index(x: Int, y: Int)
}

pub type Item {
  Item(value: Int, len: Int, start: Int, y: Int)
}

pub type Gear {
  Gear(ratio: Int)
}

fn is_digit(s: String) -> Bool {
  case string.first(s) {
    Ok("0") -> True
    Ok("1") -> True
    Ok("2") -> True
    Ok("3") -> True
    Ok("4") -> True
    Ok("5") -> True
    Ok("6") -> True
    Ok("7") -> True
    Ok("8") -> True
    Ok("9") -> True
    _ -> False
  }
}

fn get_x_indexes(line: List(String)) -> List(Int) {
  get_x_indexes_helper(line, 0, [])
}

fn get_x_indexes_helper(line: List(String), i: Int, res: List(Int)) -> List(Int) {
  let first_is_digit = line
  |> list.first
  |> result.map(is_digit)
  |> result.unwrap(False)

  case line {
    [] -> res
    [_, ..rest] if first_is_digit -> get_x_indexes_helper(rest, i + 1, res)
    [x, ..rest] if x == "." -> get_x_indexes_helper(rest, i + 1, res)
    [_, ..rest] -> get_x_indexes_helper(rest, i + 1, list.append(res, [i]))
  }
}

fn get_indexes(lst: List(List(String))) -> List(Symbol) {
  get_indexes_helper(lst, 0, [])
}

fn get_indexes_helper(lst: List(List(String)), i: Int, res: List(Symbol)) -> List(Symbol) {
  case lst {
    [] -> res
    [line, ..rest] -> {
      let xs = get_x_indexes(line)
      |> list.map(fn(x) { Index(x: x, y: i) })

      let new_res = xs
      |> list.append(res, _)

      get_indexes_helper(rest, i + 1, new_res)
    }
  }
}

fn get_line_items(line: List(String), y: Int) -> List(Item) {
  get_line_items_helper(line, y, 0, [], [])
}

fn get_line_items_helper(line: List(String), y: Int, i: Int, curr: List(String), res: List(Item)) -> List(Item) {
  let first_is_digit = line
  |> list.first
  |> result.map(is_digit)
  |> result.unwrap(False)

  case line, curr {
    [], [] -> res
    [], x -> {
      let n_digits = x
      |> list.length

      let val = x
      |> string.join("")
      |> int.parse
      |> result.unwrap(-1)

      let new_item = Item(value: val, start: i - n_digits - 1, y: y, len: n_digits)
      list.append(res, [new_item])
    }
    [x, ..rest], z if first_is_digit -> {
      let new_curr = z
      |> list.append([x])

      get_line_items_helper(rest, y, i + 1, new_curr, res)
    }
    [_, ..rest], [] -> {
      get_line_items_helper(rest, y, i + 1, curr, res)
    }
    [_, ..rest], x -> {
      let n_digits = x
      |> list.length

      let val = x
      |> string.join("")
      |> int.parse
      |> result.unwrap(-1)

      let new_item = Item(value: val, start: i - n_digits, y: y, len: n_digits)
      let new_res = res
      |> list.append([new_item])

      get_line_items_helper(rest, y, i + 1, [], new_res)
    }
  }
}

fn get_items(lst: List(List(String))) -> List(Item) {
  get_items_helper(lst, [], 0)
}

fn get_items_helper(lst: List(List(String)), res: List(Item), y: Int) -> List(Item) {
  case lst {
    [] -> res
    [line, ..rest] -> {
      let new_res = line
      |> get_line_items(y)
      |> list.append(res, _)

      get_items_helper(rest, new_res, y + 1)
    }
  }
}

fn is_symbol_of_part(item: Item, symbol: Symbol) -> Bool {
  let is_y_neighbor = {
    let diff = symbol.y - item.y
    |> int.absolute_value
    diff <= 1
  }

  let is_x_neighbor = case symbol.x, item.start {
    a, b if b >= a -> {
      let diff = a - b
      |> int.absolute_value
      diff <= 1
    }
    a, b -> {
      b + item.len >= a
    }
  }

  is_y_neighbor && is_x_neighbor
}

fn is_part(item: Item, symbols: List(Symbol)) -> Bool {
  let adjacent_symbols = symbols
  |> list.filter(fn(symbol) {
    is_symbol_of_part(item, symbol)
  })

  case adjacent_symbols {
    [] -> False
    _ -> True
  }
}

fn get_gears(parts: List(Item), symbols: List(Symbol)) -> List(Gear) {
  get_gears_helper(parts, symbols, [])
}

fn get_gears_helper(parts: List(Item), symbols: List(Symbol), res: List(Gear)) -> List(Gear) {
  case symbols {
    [] -> res
    [symbol, ..rest] -> {
      let candidates = parts
      |> list.filter(fn(part) { is_symbol_of_part(part, symbol) })

      case candidates {
        [] | [_] -> get_gears_helper(parts, rest, res)
        ps -> {
          let gear_ratio = ps
          |> list.fold(1, fn(acc, x) { acc * x.value })

          let new_res = res
          |> list.append([Gear(ratio: gear_ratio)])

          get_gears_helper(parts, rest, new_res)
        }
      }
    }
  }
}

pub fn main() {
  let input = "input.txt"
  |> simplifile.read
  |> result.map(string.split(_, "\n"))
  |> result.map(list.filter(_, fn(x) { x != "" }))

  let graphemes = input
  |> result.map(list.map(_, string.to_graphemes))

  let symbols = graphemes
  |> result.map(get_indexes)
  |> result.unwrap([])

  let parts = graphemes
  |> result.map(get_items)
  |> result.map(list.filter(_, fn(x) { is_part(x, symbols) }))
  |> result.unwrap([])

  let sum = parts
  |> list.fold(0, fn(acc, x) { acc + x.value} )
  |> int.to_string

  io.println("Part 1")
  io.println("Sum of parts: " <> sum)

  let gear_sum = get_gears(parts, symbols)
  |> list.fold(0, fn(acc, x) { acc + x.ratio })
  |> int.to_string

  io.println("Part 2")
  io.println("Sum of gear ratios: " <> gear_sum)
}
