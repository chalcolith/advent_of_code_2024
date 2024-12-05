use "collections"
use "files"
use "itertools"

actor Main
  let _env: Env
  let _xmas: String

  new create(env: Env) =>
    _env = env
    _xmas = "XMAS"

    for arg in Iter[String](env.args.values()).skip(1) do
      try
        let data = read_data(arg)?
        step_1(data)
        step_2(data)
      end
    end

  fun read_data(fname: String): Array[String] ? =>
    match OpenFile(FilePath(FileAuth(_env.root), fname))
    | let file: File =>
      Iter[String iso^](FileLines(file))
        .map[String val]({(s) => consume s})
        .collect[Array[String]]([])
    else
      _env.err.print("Unable to open " + fname)
      error
    end

  fun step_1(data: Array[String]) =>
    var num_found: USize = 0

    try
      let height = data.size()
      for y in Range(0, height) do
        let row = data(y)?
        let width = row.size()
        for x in Range(0, width) do
          if row(x)? == 'X' then
            num_found = num_found + check(data, _xmas, x, y, -1, -1) // NW
            num_found = num_found + check(data, _xmas, x, y, 0, -1) // N
            num_found = num_found + check(data, _xmas, x, y, 1, -1) // NE
            num_found = num_found + check(data, _xmas, x, y, 1, 0) // E
            num_found = num_found + check(data, _xmas, x, y, 1, 1) // SE
            num_found = num_found + check(data, _xmas, x, y, 0, 1) // S
            num_found = num_found + check(data, _xmas, x, y, -1, 1) // SW
            num_found = num_found + check(data, _xmas, x, y, -1, 0) // W
          end
        end
      end
    end

    _env.out.print("Step 1: " + num_found.string())

  fun check(
    data: Array[String], s: String, x: USize, y: USize, dx: ISize, dy: ISize)
    : USize
  =>
    try
      var x' = x
      var y' = y
      for c in s.values() do
        if data(y')?(x')? != c then
          return 0
        end
        x' = (x'.isize() + dx).usize()
        y' = (y'.isize() + dy).usize()
      end
      1
    else
      0
    end

  fun step_2(data: Array[String]) =>
    var num_found: USize = 0

    try
      let height = data.size()
      for y in Range(0, height - 2) do
        for x in Range(0, data(y)?.size() - 2) do
          if
            ((check(data, "MAS", x, y, 1, 1) == 1) or
              (check(data, "SAM", x, y, 1, 1) == 1)) and
            ((check(data, "MAS", x, y + 2, 1, -1) == 1) or
              (check(data, "SAM", x, y + 2, 1, -1) == 1))
          then
            num_found = num_found + 1
          end
        end
      end
    end

    _env.out.print("Step 2: " + num_found.string())
