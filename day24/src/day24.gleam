import gleam/io
import simplifile
import gleam/result
import gleam/list
import gleam/int
import gleam/string

pub type Hail {
  Hail(x: Int, y: Int, z: Int, dx: Int, dy: Int, dz: Int)
}

pub type Line {
  Line(m: Float, q: Float)
}

pub type Point {
  Point(x: Float, y: Float)
}

pub type Field {
  Field(start: Point, end: Point)
}

fn unwrap_at(lst: List(Int), i: Int) -> Int {
  lst
  |> list.at(i)
  |> result.unwrap(-1)
}

fn parse_hail(in: String) -> List(Hail) {
  let to_hail = fn(in: List(String)) -> Hail {
    let pos = in
    |> list.first
    |> result.unwrap("")
    |> string.split(", ")
    |> list.map(fn(x) {
      x
      |> string.trim
      |> int.parse
      |> result.unwrap(-1)
    })

    let delta = in
    |> list.last
    |> result.unwrap("")
    |> string.split(", ")
    |> list.map(fn(x) {
      x
      |> string.trim
      |> int.parse
      |> result.unwrap(-1)
    })

    Hail(unwrap_at(pos, 0), unwrap_at(pos, 1), unwrap_at(pos, 2),
         unwrap_at(delta, 0), unwrap_at(delta, 1), unwrap_at(delta, 2))
  }

  in
  |> string.split("\n")
  |> list.filter(fn(x) { !string.is_empty(x) })
  |> list.map(fn(x) {
    x
    |> string.split(" @ ")
    |> to_hail
  })
}

fn hail_to_line(hail: Hail) -> Line {
  let x0 = hail.x
  |> int.to_float

  let x1 = hail.x + hail.dx
  |> int.to_float

  let y0 = hail.y
  |> int.to_float

  let y1 = hail.y + hail.dy
  |> int.to_float

  let m = { y0 -. y1 } /. { x0 -. x1 }
  let q = y0 -. m *. x0

  Line(m, q)
}

fn get_intersection(a: Line, b: Line) -> Result(Point, Nil) {
  let delta_m = a.m -. b.m

  case delta_m {
    0.0 -> Error(Nil)
    _ -> {
      let x = { b.q -. a.q } /. delta_m
      let y = a.m *. x +. a.q

      Ok(Point(x, y))
    }
  }
}

fn point_in_field(p: Point, f: Field) -> Bool {
  p.x >=. f.start.x && p.x <=. f.end.x && p.y >=. f.start.y && p.y <=. f.end.y
}

fn get_future_crossing_points(hail: List(Hail), field: Field) -> List(Point) {
  case hail {
    [] -> []
    [x, ..rest] -> {
      let len = list.length(rest)
      let pairs = list.zip(list.repeat(x, len), rest)
      let valid_points = pairs
      |> list.map(fn(pair) {
        let #(a, b) = pair
        let p = get_intersection(hail_to_line(a), hail_to_line(b))
        case p {
          Error(_) -> Error(Nil)
          Ok(p) -> {
            let in_future = fn(h: Hail, x: Float) {
              case h.dx > 0 {
                True -> x >=. int.to_float(h.x)
                False -> x <=. int.to_float(h.x)
              }
            }

            case point_in_field(p, field) && in_future(a, p.x) && in_future(b, p.x) {
              True -> Ok(p)
              False -> Error(Nil)
            }
          }
        }
      })
      |> result.values

      let other_points = get_future_crossing_points(rest, field)
      list.append(valid_points, other_points)
    }
  }
}

pub fn main() {
  let input = "input.txt"
  |> simplifile.read
  |> result.unwrap("")

  let field = Field(Point(200000000000000.0, 200000000000000.0),
                    Point(400000000000000.0, 400000000000000.0))

  let hail = input
  |> parse_hail

  let p1 = hail
  |> get_future_crossing_points(field)
  |> list.length
  |> int.to_string

  io.println("-- Part 1 --")
  io.println("Number of future intersections: " <> p1)
}
