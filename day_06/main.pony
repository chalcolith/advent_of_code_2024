use "collections"
use "files"
use "itertools"

actor Main
  let _env: Env

  new create(env: Env) =>
    _env = env

    for arg in Iter[String](env.args.values()).skip(1) do
      try
        (let start, let dir, let width, let height, let map) = read_data(arg)?
        step_1(start, dir, width, height, map)
        step_2(start, dir, width, height, map)
      end
    end

  fun step_1(
    start: (I32, I32),
    dir: (I32, I32),
    width: I32,
    height: I32,
    map: HashSet[(I32, I32), CoordHash])
  =>
    let visited = HashMap[(I32, I32), USize, CoordHash]
    _traverse_is_loop(start, dir, width, height, map, visited)
    _env.out.print("Step 1: " + visited.size().string())

  fun step_2(
    start: (I32, I32),
    dir: (I32, I32),
    width: I32,
    height: I32,
    map: HashSet[(I32, I32), CoordHash])
  =>
    let visited = HashMap[(I32, I32), USize, CoordHash]
    var num_obstructions: USize = 0
    for y in Range[I32](0, height) do
      for x in Range[I32](0, width) do
        if (x == start._1) and (y == start._2) then continue end
        if map.contains((x, y)) then continue end

        map.set((x, y))
        if _traverse_is_loop(start, dir, width, height, map, visited) then
          num_obstructions = num_obstructions + 1
        end
        map.unset((x, y))
      end
    end
    _env.out.print("Step 2: " + num_obstructions.string())

  fun _traverse_is_loop(
    start: (I32, I32),
    dir: (I32, I32),
    width: I32,
    height: I32,
    map: HashSet[(I32, I32), CoordHash],
    visited: HashMap[(I32, I32), USize, CoordHash])
    : Bool
  =>
    var cur = start
    var delta = dir
    let blocks_by_dir =
      HashMap[(I32, I32), HashMap[(I32, I32), USize, CoordHash], CoordHash]

    while true do
      visited.upsert(cur, 1, {(n, m) => n + m})

      let next = (cur._1 + delta._1, cur._2 + delta._2)

      if
        (next._1 < 0) or
        (next._1 >= width) or
        (next._2 < 0) or
        (next._2 >= height)
      then
        break
      elseif map.contains(next) then
        let by_dir =
          match try blocks_by_dir(next)? end
          | let by_dir': HashMap[(I32, I32), USize, CoordHash] =>
            by_dir'
          else
            let by_dir' = HashMap[(I32, I32), USize, CoordHash]
            blocks_by_dir(next) = by_dir'
            by_dir'
          end
        if by_dir.upsert(delta, 1, {(n, m) => n + m}) >= 2 then
          return true // we're in a loop
        end

        delta =
          match delta
          | (0, -1) => (1, 0)
          | (1, 0) => (0, 1)
          | (0, 1) => (-1, 0)
          | (-1, 0) => (0, -1)
          else
            _env.out.print("Error in dir")
            dir
          end
        // we need to check for a blocker in the new direction
      else
        cur = (cur._1 + delta._1, cur._2 + delta._2)
      end
    end
    false

  fun read_data(fname: String)
    : ((I32, I32), (I32, I32), I32, I32, HashSet[(I32, I32), CoordHash]) ?
  =>
    match OpenFile(FilePath(FileAuth(_env.root), fname))
    | let file: File =>
      let after = Map[USize, Set[USize]]
      let updates = Array[Array[USize]]

      var y: I32 = 0
      var x: I32 = 0

      var start: (I32, I32) = (0, 0)
      var dir: (I32, I32) = (-1, 0)
      var width: I32 = 0
      var height: I32 = 0
      let map = HashSet[(I32, I32), CoordHash]

      for line in FileLines(file) do
        if line.size().i32() > width then
          width = line.size().i32()
        end

        x = 0
        for i in Range(0, line.size()) do
          try
            let ch = line(i)?
            match ch
            | '#' =>
              map.set((x, y))
            | '^' =>
              start = (x, y)
              dir = (0, -1)
            | '>' =>
              start = (x, y)
              dir = (1, 0)
            | 'V' =>
              start = (x, y)
              dir = (0, 1)
            | '<' =>
              start = (x, y)
              dir = (-1, 0)
            end
          end
          x = x + 1
        end
        y = y + 1
      end
      height = y

      (start, dir, width, height, map)
    else
      _env.err.print("Unable to open " + fname)
      error
    end

primitive CoordHash is HashFunction[(I32, I32)]
  fun box hash(x: box->(I32, I32)): USize =>
    (x._1 xor x._2).usize()

  fun box eq(x: box->(I32, I32), y: box->(I32, I32)): Bool =>
    (x._1 == y._1) and (x._2 == y._2)
