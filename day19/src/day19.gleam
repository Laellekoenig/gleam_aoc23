import gleam/io
import simplifile
import gleam/list
import gleam/int
import gleam/string
import gleam/result
import gleam/dict.{type Dict}

pub type Part {
  Part(x: Int, m: Int, a: Int, s: Int)
}

pub type Action {
  Accept
  Reject
  Goto(target: String)
}

pub type Rule {
  Rule(cond: fn(Part) -> Bool, action: Action)
}

pub type Workflow {
  Workflow(name: String, rules: List(Rule))
}

fn parse_parts(in: String) -> List(Part) {
  in
  |> string.split("\n")
  |> list.filter(fn(x) { !string.is_empty(x) })
  |> list.map(fn(x) {
    x
    |> string.drop_left(1)
    |> string.drop_right(1)
  })
  |> list.map(fn(x) {
    let nums = x
    |> string.split(",")
    |> list.map(fn(x) {
      x
      |> string.split("=" )
      |> list.last
      |> result.map(int.parse)
      |> result.flatten
      |> result.unwrap(-1)
    })

    let get_num = fn(lst, i) {
      lst
      |> list.at(i)
      |> result.unwrap(-1)
    }

    Part(get_num(nums, 0), get_num(nums, 1), get_num(nums, 2), get_num(nums, 3))
  })
}

fn parse_rule(in: String) -> Rule {
  case in {
    "A" -> Rule(fn(_) { True }, Accept)
    "R" -> Rule(fn(_) { True }, Reject)
    _ -> case string.contains(in, ":") {
      False -> Rule(fn(_) { True }, Goto(in))
      True -> {
        let split = in
        |> string.split("<")
        |> list.map(fn(x) { string.split(x, ">") })
        |> list.flatten

        let cat = split
        |> list.first
        |> result.unwrap("")

        let split = split
        |> list.last
        |> result.unwrap("")
        |> string.split(":")

        let value = split
        |> list.first
        |> result.map(int.parse)
        |> result.flatten
        |> result.unwrap(-1)

        let target = split
        |> list.last
        |> result.unwrap("")

        let get_cat_val = fn(c: String, p: Part) {
          case c {
            "x" -> p.x
            "m" -> p.m
            "a" -> p.a
            "s" -> p.s
            _ -> panic as "Invalid category"
          }
        }

        let cond = case string.contains(in, "<") {
          True -> fn(x: Part) { get_cat_val(cat, x) < value }
          False -> fn(x: Part) { get_cat_val(cat, x) > value }
        }

        case target {
          "A" -> Rule(cond, Accept)
          "R" -> Rule(cond, Reject)
          _ -> Rule(cond, Goto(target))
        }
      }
    }
  }
}

fn parse_workflow(in: String) -> Workflow {
  let split = in
  |> string.split("{")

  let name = split
  |> list.first
  |> result.unwrap("")

  let rules = split
  |> list.last
  |> result.unwrap("")
  |> string.drop_right(1)
  |> string.split(",")
  |> list.map(parse_rule)

  Workflow(name, rules)
}

fn parse_workflows(in: String) -> Dict(String, Workflow) {
  in
  |> string.split("\n")
  |> list.map(fn(x) {
    let workflow = parse_workflow(x)
    #(workflow.name, workflow)
  })
  |> dict.from_list
}

fn is_accepted(part: Part, workflows: Dict(String, Workflow)) -> Bool {
  let start = dict.get(workflows, "in")
  case start {
    Ok(start) -> is_accepted_helper(part, start, 0, workflows)
    _ -> panic as "Invalid state"
  }
}

fn is_accepted_helper(part: Part, curr: Workflow, i: Int, workflows: Dict(String, Workflow)) -> Bool {
  let rule = list.at(curr.rules, i)
  case rule {
    Ok(rule) -> {
      case rule.cond(part) {
        False -> is_accepted_helper(part, curr, i + 1, workflows)
        True -> {
          case rule.action {
            Accept -> True
            Reject -> False
            Goto(target) -> {
              let new_curr = dict.get(workflows, target)
              case new_curr {
                Ok(new_curr) -> is_accepted_helper(part, new_curr, 0, workflows)
                _ -> panic as "Invalid state"
              }
            }
          }
        }
      }
    }
    _ -> panic as "Invalid state"
  }
}

pub fn main() {
  let input = "input.txt"
  |> simplifile.read
  |> result.unwrap("")

  let split = input
  |> string.split("\n\n")

  let workflows = split
  |> list.first
  |> result.unwrap("")
  |> parse_workflows

  let parts = split
  |> list.last
  |> result.unwrap("")
  |> parse_parts

  let p1 = parts
  |> list.filter(fn(x) { is_accepted(x, workflows) })
  |> list.fold(0, fn(acc, x) { acc + x.x + x.m + x.a + x.s })
  |> int.to_string

  io.println("-- Part 1 --")
  io.println("Sum: " <> p1)
}
