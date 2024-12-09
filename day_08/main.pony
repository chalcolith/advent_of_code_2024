use "collections"
use "files"
use "itertools"

actor Main
  let _env: Env

  new create(env: Env) =>
    _env = env

    for arg in Iter[String](env.args.values()).skip(1) do
      try
        (let width, let height, let antennas) = read_data(arg)?
        step_1(width, height, antennas)
        step_2(width, height, antennas)
      end
    end

  fun step_1(width: I32, height: I32, antennas: Map[U8, Array[(I32, I32)]]) =>
    let num = count_antinodes(width, height, antennas, false)
    _env.out.print("Step 1: " + num.string())

  fun step_2(width: I32, height: I32, antennas: Map[U8, Array[(I32, I32)]]) =>
    let num = count_antinodes(width, height, antennas, true)
    _env.out.print("Step 2: " + num.string())

  fun count_antinodes(
    width: I32,
    height: I32,
    antennas: Map[U8, Array[(I32, I32)]],
    all: Bool)
    : USize
  =>
    let antinodes = HashSet[(I32, I32), CoordHash]
    for (ch, coords) in antennas.pairs() do
      for i in Range(0, coords.size()) do
        for j in Range(0, coords.size()) do
          if j == i then continue end
          try
            (let ax, let ay) = coords(i)?
            (let bx, let by) = coords(j)?
            let dx = ax - bx
            let dy = ay - by

            if all then
              var x' = ax
              var y' = ay
              while
                ((x' >= 0) and (x' < width)) and ((y' >= 0) and (y' < height))
              do
                antinodes.set((x', y'))
                x' = x' + dx
                y' = y' + dy
              end
            else
              let x' = ax + dx
              let y' = ay + dy
              if
                ((x' >= 0) and (x' < width)) and ((y' >= 0) and (y' < height))
              then
                antinodes.set((x', y'))
              end
            end
          end
        end
      end
    end
    antinodes.size()

  fun read_data(fname: String): (I32, I32, Map[U8, Array[(I32, I32)]]) ? =>
    match OpenFile(FilePath(FileAuth(_env.root), fname))
    | let file: File =>
      var width: I32 = 0
      var y: I32 = 0
      let antennas = Map[U8, Array[(I32, I32)]]
      for line in FileLines(file) do
        if width < line.size().i32() then
          width = line.size().i32()
        end
        var x: I32 = 0
        for ch in (consume line).values() do
          if
            ((ch >= '0') and (ch <= '9')) or
            ((ch >= 'a') and (ch <= 'z')) or
            ((ch >= 'A') and (ch <= 'Z'))
          then
            let coords =
              match try antennas(ch)? end
              | let coords': Array[(I32, I32)] =>
                coords'
              else
                let coords' = Array[(I32, I32)]
                antennas(ch) = coords'
                coords'
              end
            coords.push((x, y))
          end
          x = x + 1
        end
        y = y + 1
      end
      (width, y, antennas)
    else
      _env.err.print("Unable to open " + fname)
      error
    end

primitive CoordHash is HashFunction[(I32, I32)]
  fun box hash(x: box->(I32, I32)): USize =>
    (x._1 xor x._2).usize()

  fun box eq(x: box->(I32, I32), y: box->(I32, I32)): Bool =>
    (x._1 == y._1) and (x._2 == y._2)
