use "collections"
use "files"
use "itertools"

actor Main
  let _env: Env

  new create(env: Env) =>
    _env = env

    for arg in Iter[String](env.args.values()).skip(1) do
      try
        let map = read_data(arg)?
        (let ends, let paths) = process(map)
        _env.out.print("Step 1: " + ends.string())
        _env.out.print("Step 2: " + paths.string())
      end
    end

  fun process(map: Array[Array[U8]]): (USize, USize) =>
    var num_ends: USize = 0
    var num_paths: USize = 0
    try
      let ends = HashSet[(USize, USize), CoordHash]
      let count = [as USize: 0 ]
      for (y, row) in map.pairs() do
        for (x, _) in row.pairs() do
          if map(y)?(x)? == 0 then
            ends.clear()
            count(0)? = 0
            _process_aux(x, y, map, count, ends)
            num_ends = num_ends + ends.size()
            num_paths = num_paths + count(0)?
          end
        end
      end
    end
    (num_ends, num_paths)

  fun _process_aux(
    x: USize,
    y: USize,
    map: Array[Array[U8]],
    count: Array[USize],
    ends: HashSet[(USize, USize), CoordHash])
  =>
    try
      let h = map(y)?(x)?
      if h == 9 then
        count(0)? = count(0)? + 1
        ends.set((x, y))
      else
        let height = map.size()
        let width = map(0)?.size()
        if (y > 0) and (map(y - 1)?(x)? == (h + 1)) then // up
          _process_aux(x, y - 1, map, count, ends)
        end
        if (x < (width - 1)) and (map(y)?(x + 1)? == (h + 1)) then // right
          _process_aux(x + 1, y, map, count, ends)
        end
        if (y < (height - 1)) and (map(y + 1)?(x)? == (h + 1)) then // down
          _process_aux(x, y + 1, map, count, ends)
        end
        if (x > 0) and (map(y)?(x - 1)? == (h + 1)) then // left
          _process_aux(x - 1, y, map, count, ends)
        end
      end
    end

  fun read_data(fname: String): Array[Array[U8]] ? =>
    match OpenFile(FilePath(FileAuth(_env.root), fname))
    | let file: File =>
      let result = Array[Array[U8]]
      for line in FileLines(file) do
        let arr = Array[U8]
        for ch in (consume line).values() do
          arr.push(ch - '0')
        end
        result.push(arr)
      end
      result
    else
      _env.err.print("Unable to open " + fname)
      error
    end

primitive CoordHash is HashFunction[(USize, USize)]
  fun box hash(x: box->(USize, USize)): USize =>
    (x._1 xor x._2).usize()

  fun box eq(x: box->(USize, USize), y: box->(USize, USize)): Bool =>
    (x._1 == y._1) and (x._2 == y._2)
