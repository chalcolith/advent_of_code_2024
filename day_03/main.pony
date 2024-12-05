use "files"
use "itertools"
use "regex"

actor Main
  let _env: Env

  new create(env: Env) =>
    _env = env
    let mul =
      try
        Regex("""mul\((\d+),(\d+)\)|(do\(\))|(don't\(\))""")?
      else
        _env.err.print("Invalid regex")
        return
      end

    for arg in Iter[String](env.args.values()).skip(1) do
      try
        let data = recover val read_data(arg)? end
        step_1(mul, data)
        step_2(mul, data)
      end
    end

  fun read_data(fname: String): String iso^ ? =>
    match OpenFile(FilePath(FileAuth(_env.root), fname))
    | let file: File =>
      file.read_string(file.size())
    else
      _env.err.print("Unable to open " + fname)
      error
    end

  fun step_1(rx: Regex, data: String) =>
    let sum_of_muls =
      Iter[Match](rx.matches(data))
        .filter_map[I32]({(m) => try m(1)?.i32()? * m(2)?.i32()? end })
        .fold[I32](0, {(acc, n) => acc + n})
    _env.out.print("Step 1: " + sum_of_muls.string())

  fun step_2(rx: Regex, data: String) =>
    (let sum_of_muls, _) =
      Iter[Match](rx.matches(data))
        .fold[(I32, Bool)](
          (0, true),
          {(acc, m) =>
            try
              (let sum, let process) = acc
              let groups = m.groups()
              if groups(2)?.size() > 0 then
                return (sum, true)
              elseif groups(3)?.size() > 0 then
                return (sum, false)
              elseif process then
                return (sum + (groups(0)?.i32()? * groups(1)?.i32()?), process)
              end
            end
            acc
          })
    _env.out.print("Step 2: " + sum_of_muls.string())
