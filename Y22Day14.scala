import scala.annotation.tailrec

case class Coord(
                x: Int,
                y: Int
                ) {
  def down = Coord(x, y+1)
  def left = Coord(x-1, y+1)
  def right = Coord(x+1, y+1)
}

object DayFourteen extends App {

  val testInput = "498,4 -> 498,6 -> 496,6¬503,4 -> 502,4 -> 502,9 -> 494,9"
  val input = "470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬432,82 -> 450,82 -> 450,81¬466,59 -> 466,61 -> 461,61 -> 461,66 -> 480,66 -> 480,61 -> 472,61 -> 472,59¬406,118 -> 406,110 -> 406,118 -> 408,118 -> 408,115 -> 408,118 -> 410,118 -> 410,115 -> 410,118 -> 412,118 -> 412,110 -> 412,118 -> 414,118 -> 414,111 -> 414,118 -> 416,118 -> 416,114 -> 416,118 -> 418,118 -> 418,110 -> 418,118¬427,88 -> 427,92 -> 423,92 -> 423,98 -> 440,98 -> 440,92 -> 432,92 -> 432,88¬480,42 -> 480,43 -> 489,43 -> 489,42¬489,28 -> 489,31 -> 484,31 -> 484,37 -> 500,37 -> 500,31 -> 492,31 -> 492,28¬406,118 -> 406,110 -> 406,118 -> 408,118 -> 408,115 -> 408,118 -> 410,118 -> 410,115 -> 410,118 -> 412,118 -> 412,110 -> 412,118 -> 414,118 -> 414,111 -> 414,118 -> 416,118 -> 416,114 -> 416,118 -> 418,118 -> 418,110 -> 418,118¬406,118 -> 406,110 -> 406,118 -> 408,118 -> 408,115 -> 408,118 -> 410,118 -> 410,115 -> 410,118 -> 412,118 -> 412,110 -> 412,118 -> 414,118 -> 414,111 -> 414,118 -> 416,118 -> 416,114 -> 416,118 -> 418,118 -> 418,110 -> 418,118¬403,121 -> 403,125 -> 395,125 -> 395,130 -> 412,130 -> 412,125 -> 406,125 -> 406,121¬378,152 -> 378,147 -> 378,152 -> 380,152 -> 380,148 -> 380,152 -> 382,152 -> 382,145 -> 382,152 -> 384,152 -> 384,145 -> 384,152 -> 386,152 -> 386,147 -> 386,152 -> 388,152 -> 388,151 -> 388,152 -> 390,152 -> 390,144 -> 390,152¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬414,105 -> 418,105¬406,118 -> 406,110 -> 406,118 -> 408,118 -> 408,115 -> 408,118 -> 410,118 -> 410,115 -> 410,118 -> 412,118 -> 412,110 -> 412,118 -> 414,118 -> 414,111 -> 414,118 -> 416,118 -> 416,114 -> 416,118 -> 418,118 -> 418,110 -> 418,118¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬427,88 -> 427,92 -> 423,92 -> 423,98 -> 440,98 -> 440,92 -> 432,92 -> 432,88¬378,152 -> 378,147 -> 378,152 -> 380,152 -> 380,148 -> 380,152 -> 382,152 -> 382,145 -> 382,152 -> 384,152 -> 384,145 -> 384,152 -> 386,152 -> 386,147 -> 386,152 -> 388,152 -> 388,151 -> 388,152 -> 390,152 -> 390,144 -> 390,152¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬466,59 -> 466,61 -> 461,61 -> 461,66 -> 480,66 -> 480,61 -> 472,61 -> 472,59¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬489,28 -> 489,31 -> 484,31 -> 484,37 -> 500,37 -> 500,31 -> 492,31 -> 492,28¬378,152 -> 378,147 -> 378,152 -> 380,152 -> 380,148 -> 380,152 -> 382,152 -> 382,145 -> 382,152 -> 384,152 -> 384,145 -> 384,152 -> 386,152 -> 386,147 -> 386,152 -> 388,152 -> 388,151 -> 388,152 -> 390,152 -> 390,144 -> 390,152¬491,19 -> 496,19¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬427,88 -> 427,92 -> 423,92 -> 423,98 -> 440,98 -> 440,92 -> 432,92 -> 432,88¬378,152 -> 378,147 -> 378,152 -> 380,152 -> 380,148 -> 380,152 -> 382,152 -> 382,145 -> 382,152 -> 384,152 -> 384,145 -> 384,152 -> 386,152 -> 386,147 -> 386,152 -> 388,152 -> 388,151 -> 388,152 -> 390,152 -> 390,144 -> 390,152¬406,118 -> 406,110 -> 406,118 -> 408,118 -> 408,115 -> 408,118 -> 410,118 -> 410,115 -> 410,118 -> 412,118 -> 412,110 -> 412,118 -> 414,118 -> 414,111 -> 414,118 -> 416,118 -> 416,114 -> 416,118 -> 418,118 -> 418,110 -> 418,118¬449,74 -> 453,74¬406,118 -> 406,110 -> 406,118 -> 408,118 -> 408,115 -> 408,118 -> 410,118 -> 410,115 -> 410,118 -> 412,118 -> 412,110 -> 412,118 -> 414,118 -> 414,111 -> 414,118 -> 416,118 -> 416,114 -> 416,118 -> 418,118 -> 418,110 -> 418,118¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬513,25 -> 518,25¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬406,118 -> 406,110 -> 406,118 -> 408,118 -> 408,115 -> 408,118 -> 410,118 -> 410,115 -> 410,118 -> 412,118 -> 412,110 -> 412,118 -> 414,118 -> 414,111 -> 414,118 -> 416,118 -> 416,114 -> 416,118 -> 418,118 -> 418,110 -> 418,118¬427,88 -> 427,92 -> 423,92 -> 423,98 -> 440,98 -> 440,92 -> 432,92 -> 432,88¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬378,152 -> 378,147 -> 378,152 -> 380,152 -> 380,148 -> 380,152 -> 382,152 -> 382,145 -> 382,152 -> 384,152 -> 384,145 -> 384,152 -> 386,152 -> 386,147 -> 386,152 -> 388,152 -> 388,151 -> 388,152 -> 390,152 -> 390,144 -> 390,152¬480,42 -> 480,43 -> 489,43 -> 489,42¬403,121 -> 403,125 -> 395,125 -> 395,130 -> 412,130 -> 412,125 -> 406,125 -> 406,121¬378,152 -> 378,147 -> 378,152 -> 380,152 -> 380,148 -> 380,152 -> 382,152 -> 382,145 -> 382,152 -> 384,152 -> 384,145 -> 384,152 -> 386,152 -> 386,147 -> 386,152 -> 388,152 -> 388,151 -> 388,152 -> 390,152 -> 390,144 -> 390,152¬498,19 -> 503,19¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬466,59 -> 466,61 -> 461,61 -> 461,66 -> 480,66 -> 480,61 -> 472,61 -> 472,59¬378,152 -> 378,147 -> 378,152 -> 380,152 -> 380,148 -> 380,152 -> 382,152 -> 382,145 -> 382,152 -> 384,152 -> 384,145 -> 384,152 -> 386,152 -> 386,147 -> 386,152 -> 388,152 -> 388,151 -> 388,152 -> 390,152 -> 390,144 -> 390,152¬502,22 -> 507,22¬378,152 -> 378,147 -> 378,152 -> 380,152 -> 380,148 -> 380,152 -> 382,152 -> 382,145 -> 382,152 -> 384,152 -> 384,145 -> 384,152 -> 386,152 -> 386,147 -> 386,152 -> 388,152 -> 388,151 -> 388,152 -> 390,152 -> 390,144 -> 390,152¬403,121 -> 403,125 -> 395,125 -> 395,130 -> 412,130 -> 412,125 -> 406,125 -> 406,121¬509,22 -> 514,22¬506,25 -> 511,25¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬427,88 -> 427,92 -> 423,92 -> 423,98 -> 440,98 -> 440,92 -> 432,92 -> 432,88¬499,25 -> 504,25¬505,19 -> 510,19¬495,22 -> 500,22¬443,78 -> 447,78¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬406,118 -> 406,110 -> 406,118 -> 408,118 -> 408,115 -> 408,118 -> 410,118 -> 410,115 -> 410,118 -> 412,118 -> 412,110 -> 412,118 -> 414,118 -> 414,111 -> 414,118 -> 416,118 -> 416,114 -> 416,118 -> 418,118 -> 418,110 -> 418,118¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬403,121 -> 403,125 -> 395,125 -> 395,130 -> 412,130 -> 412,125 -> 406,125 -> 406,121¬406,118 -> 406,110 -> 406,118 -> 408,118 -> 408,115 -> 408,118 -> 410,118 -> 410,115 -> 410,118 -> 412,118 -> 412,110 -> 412,118 -> 414,118 -> 414,111 -> 414,118 -> 416,118 -> 416,114 -> 416,118 -> 418,118 -> 418,110 -> 418,118¬488,22 -> 493,22¬417,103 -> 421,103¬403,121 -> 403,125 -> 395,125 -> 395,130 -> 412,130 -> 412,125 -> 406,125 -> 406,121¬494,16 -> 499,16¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬446,76 -> 450,76¬489,28 -> 489,31 -> 484,31 -> 484,37 -> 500,37 -> 500,31 -> 492,31 -> 492,28¬406,118 -> 406,110 -> 406,118 -> 408,118 -> 408,115 -> 408,118 -> 410,118 -> 410,115 -> 410,118 -> 412,118 -> 412,110 -> 412,118 -> 414,118 -> 414,111 -> 414,118 -> 416,118 -> 416,114 -> 416,118 -> 418,118 -> 418,110 -> 418,118¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬378,152 -> 378,147 -> 378,152 -> 380,152 -> 380,148 -> 380,152 -> 382,152 -> 382,145 -> 382,152 -> 384,152 -> 384,145 -> 384,152 -> 386,152 -> 386,147 -> 386,152 -> 388,152 -> 388,151 -> 388,152 -> 390,152 -> 390,144 -> 390,152¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬378,152 -> 378,147 -> 378,152 -> 380,152 -> 380,148 -> 380,152 -> 382,152 -> 382,145 -> 382,152 -> 384,152 -> 384,145 -> 384,152 -> 386,152 -> 386,147 -> 386,152 -> 388,152 -> 388,151 -> 388,152 -> 390,152 -> 390,144 -> 390,152¬466,59 -> 466,61 -> 461,61 -> 461,66 -> 480,66 -> 480,61 -> 472,61 -> 472,59¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬501,16 -> 506,16¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬466,59 -> 466,61 -> 461,61 -> 461,66 -> 480,66 -> 480,61 -> 472,61 -> 472,59¬430,84 -> 430,85 -> 436,85¬426,105 -> 430,105¬452,70 -> 452,71 -> 465,71 -> 465,70¬485,25 -> 490,25¬489,28 -> 489,31 -> 484,31 -> 484,37 -> 500,37 -> 500,31 -> 492,31 -> 492,28¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬406,118 -> 406,110 -> 406,118 -> 408,118 -> 408,115 -> 408,118 -> 410,118 -> 410,115 -> 410,118 -> 412,118 -> 412,110 -> 412,118 -> 414,118 -> 414,111 -> 414,118 -> 416,118 -> 416,114 -> 416,118 -> 418,118 -> 418,110 -> 418,118¬406,118 -> 406,110 -> 406,118 -> 408,118 -> 408,115 -> 408,118 -> 410,118 -> 410,115 -> 410,118 -> 412,118 -> 412,110 -> 412,118 -> 414,118 -> 414,111 -> 414,118 -> 416,118 -> 416,114 -> 416,118 -> 418,118 -> 418,110 -> 418,118¬455,78 -> 459,78¬406,118 -> 406,110 -> 406,118 -> 408,118 -> 408,115 -> 408,118 -> 410,118 -> 410,115 -> 410,118 -> 412,118 -> 412,110 -> 412,118 -> 414,118 -> 414,111 -> 414,118 -> 416,118 -> 416,114 -> 416,118 -> 418,118 -> 418,110 -> 418,118¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬466,59 -> 466,61 -> 461,61 -> 461,66 -> 480,66 -> 480,61 -> 472,61 -> 472,59¬403,121 -> 403,125 -> 395,125 -> 395,130 -> 412,130 -> 412,125 -> 406,125 -> 406,121¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬423,103 -> 427,103¬480,42 -> 480,43 -> 489,43 -> 489,42¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬406,118 -> 406,110 -> 406,118 -> 408,118 -> 408,115 -> 408,118 -> 410,118 -> 410,115 -> 410,118 -> 412,118 -> 412,110 -> 412,118 -> 414,118 -> 414,111 -> 414,118 -> 416,118 -> 416,114 -> 416,118 -> 418,118 -> 418,110 -> 418,118¬452,76 -> 456,76¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬378,152 -> 378,147 -> 378,152 -> 380,152 -> 380,148 -> 380,152 -> 382,152 -> 382,145 -> 382,152 -> 384,152 -> 384,145 -> 384,152 -> 386,152 -> 386,147 -> 386,152 -> 388,152 -> 388,151 -> 388,152 -> 390,152 -> 390,144 -> 390,152¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬378,152 -> 378,147 -> 378,152 -> 380,152 -> 380,148 -> 380,152 -> 382,152 -> 382,145 -> 382,152 -> 384,152 -> 384,145 -> 384,152 -> 386,152 -> 386,147 -> 386,152 -> 388,152 -> 388,151 -> 388,152 -> 390,152 -> 390,144 -> 390,152¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬430,84 -> 430,85 -> 436,85¬452,70 -> 452,71 -> 465,71 -> 465,70¬378,152 -> 378,147 -> 378,152 -> 380,152 -> 380,148 -> 380,152 -> 382,152 -> 382,145 -> 382,152 -> 384,152 -> 384,145 -> 384,152 -> 386,152 -> 386,147 -> 386,152 -> 388,152 -> 388,151 -> 388,152 -> 390,152 -> 390,144 -> 390,152¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬395,136 -> 399,136¬420,101 -> 424,101¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬389,136 -> 393,136¬427,88 -> 427,92 -> 423,92 -> 423,98 -> 440,98 -> 440,92 -> 432,92 -> 432,88¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬427,88 -> 427,92 -> 423,92 -> 423,98 -> 440,98 -> 440,92 -> 432,92 -> 432,88¬398,139 -> 402,139¬489,28 -> 489,31 -> 484,31 -> 484,37 -> 500,37 -> 500,31 -> 492,31 -> 492,28¬466,59 -> 466,61 -> 461,61 -> 461,66 -> 480,66 -> 480,61 -> 472,61 -> 472,59¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬452,70 -> 452,71 -> 465,71 -> 465,70¬420,105 -> 424,105¬378,152 -> 378,147 -> 378,152 -> 380,152 -> 380,148 -> 380,152 -> 382,152 -> 382,145 -> 382,152 -> 384,152 -> 384,145 -> 384,152 -> 386,152 -> 386,147 -> 386,152 -> 388,152 -> 388,151 -> 388,152 -> 390,152 -> 390,144 -> 390,152¬378,152 -> 378,147 -> 378,152 -> 380,152 -> 380,148 -> 380,152 -> 382,152 -> 382,145 -> 382,152 -> 384,152 -> 384,145 -> 384,152 -> 386,152 -> 386,147 -> 386,152 -> 388,152 -> 388,151 -> 388,152 -> 390,152 -> 390,144 -> 390,152¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬449,78 -> 453,78¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬489,28 -> 489,31 -> 484,31 -> 484,37 -> 500,37 -> 500,31 -> 492,31 -> 492,28¬406,118 -> 406,110 -> 406,118 -> 408,118 -> 408,115 -> 408,118 -> 410,118 -> 410,115 -> 410,118 -> 412,118 -> 412,110 -> 412,118 -> 414,118 -> 414,111 -> 414,118 -> 416,118 -> 416,114 -> 416,118 -> 418,118 -> 418,110 -> 418,118¬492,25 -> 497,25¬406,118 -> 406,110 -> 406,118 -> 408,118 -> 408,115 -> 408,118 -> 410,118 -> 410,115 -> 410,118 -> 412,118 -> 412,110 -> 412,118 -> 414,118 -> 414,111 -> 414,118 -> 416,118 -> 416,114 -> 416,118 -> 418,118 -> 418,110 -> 418,118¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬406,118 -> 406,110 -> 406,118 -> 408,118 -> 408,115 -> 408,118 -> 410,118 -> 410,115 -> 410,118 -> 412,118 -> 412,110 -> 412,118 -> 414,118 -> 414,111 -> 414,118 -> 416,118 -> 416,114 -> 416,118 -> 418,118 -> 418,110 -> 418,118¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬378,152 -> 378,147 -> 378,152 -> 380,152 -> 380,148 -> 380,152 -> 382,152 -> 382,145 -> 382,152 -> 384,152 -> 384,145 -> 384,152 -> 386,152 -> 386,147 -> 386,152 -> 388,152 -> 388,151 -> 388,152 -> 390,152 -> 390,144 -> 390,152¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬406,118 -> 406,110 -> 406,118 -> 408,118 -> 408,115 -> 408,118 -> 410,118 -> 410,115 -> 410,118 -> 412,118 -> 412,110 -> 412,118 -> 414,118 -> 414,111 -> 414,118 -> 416,118 -> 416,114 -> 416,118 -> 418,118 -> 418,110 -> 418,118¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬392,133 -> 396,133¬497,13 -> 502,13¬378,152 -> 378,147 -> 378,152 -> 380,152 -> 380,148 -> 380,152 -> 382,152 -> 382,145 -> 382,152 -> 384,152 -> 384,145 -> 384,152 -> 386,152 -> 386,147 -> 386,152 -> 388,152 -> 388,151 -> 388,152 -> 390,152 -> 390,144 -> 390,152¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬432,82 -> 450,82 -> 450,81¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬386,139 -> 390,139¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬368,165 -> 368,164 -> 368,165 -> 370,165 -> 370,164 -> 370,165 -> 372,165 -> 372,163 -> 372,165 -> 374,165 -> 374,161 -> 374,165 -> 376,165 -> 376,157 -> 376,165 -> 378,165 -> 378,164 -> 378,165 -> 380,165 -> 380,162 -> 380,165 -> 382,165 -> 382,163 -> 382,165 -> 384,165 -> 384,158 -> 384,165¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬378,152 -> 378,147 -> 378,152 -> 380,152 -> 380,148 -> 380,152 -> 382,152 -> 382,145 -> 382,152 -> 384,152 -> 384,145 -> 384,152 -> 386,152 -> 386,147 -> 386,152 -> 388,152 -> 388,151 -> 388,152 -> 390,152 -> 390,144 -> 390,152¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬392,139 -> 396,139¬406,118 -> 406,110 -> 406,118 -> 408,118 -> 408,115 -> 408,118 -> 410,118 -> 410,115 -> 410,118 -> 412,118 -> 412,110 -> 412,118 -> 414,118 -> 414,111 -> 414,118 -> 416,118 -> 416,114 -> 416,118 -> 418,118 -> 418,110 -> 418,118¬403,121 -> 403,125 -> 395,125 -> 395,130 -> 412,130 -> 412,125 -> 406,125 -> 406,121¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬470,56 -> 470,54 -> 470,56 -> 472,56 -> 472,46 -> 472,56 -> 474,56 -> 474,49 -> 474,56 -> 476,56 -> 476,49 -> 476,56 -> 478,56 -> 478,54 -> 478,56 -> 480,56 -> 480,46 -> 480,56 -> 482,56 -> 482,52 -> 482,56 -> 484,56 -> 484,53 -> 484,56 -> 486,56 -> 486,47 -> 486,56¬489,28 -> 489,31 -> 484,31 -> 484,37 -> 500,37 -> 500,31 -> 492,31 -> 492,28¬406,118 -> 406,110 -> 406,118 -> 408,118 -> 408,115 -> 408,118 -> 410,118 -> 410,115 -> 410,118 -> 412,118 -> 412,110 -> 412,118 -> 414,118 -> 414,111 -> 414,118 -> 416,118 -> 416,114 -> 416,118 -> 418,118 -> 418,110 -> 418,118¬378,152 -> 378,147 -> 378,152 -> 380,152 -> 380,148 -> 380,152 -> 382,152 -> 382,145 -> 382,152 -> 384,152 -> 384,145 -> 384,152 -> 386,152 -> 386,147 -> 386,152 -> 388,152 -> 388,151 -> 388,152 -> 390,152 -> 390,144 -> 390,152¬378,152 -> 378,147 -> 378,152 -> 380,152 -> 380,148 -> 380,152 -> 382,152 -> 382,145 -> 382,152 -> 384,152 -> 384,145 -> 384,152 -> 386,152 -> 386,147 -> 386,152 -> 388,152 -> 388,151 -> 388,152 -> 390,152 -> 390,144 -> 390,152"

  def fillGap(coordA: Coord, coordB: Coord): Seq[Coord] = {
    if (coordA.x == coordB.x) (Math.min(coordA.y, coordB.y) to Math.max(coordA.y, coordB.y)).map(Coord(coordA.x, _))
    else (Math.min(coordA.x, coordB.x) to Math.max(coordA.x, coordB.x)).map(Coord(_, coordA.y))
  }

  def formRocks(formation: String) = {
    val rockCorners = formation.split(" -> ").map(_.split(",")).map(coord => Coord(coord(0).toInt, coord(1).toInt)).zipWithIndex
    rockCorners.tail.flatMap(corner => fillGap(corner._1, rockCorners(corner._2-1)._1))
  }

  val rocks = input.split("¬").flatMap(formRocks).distinct
  val lowestRock = rocks.map(_.y).max
  val floor = fillGap(Coord(500-lowestRock,lowestRock + 2), Coord(500+lowestRock,lowestRock + 2))
  val rocksAndFloor = (rocks ++ floor).distinct

  @tailrec
  def simulateGrainOfSand(rocks: Seq[Coord], sand: Coord = Coord(500, 0)): Coord = {
    val nextSand = if (!rocks.contains(sand.down)) Some(sand.down)
      else if (!rocks.contains(sand.left)) Some(sand.left)
      else if (!rocks.contains(sand.right)) Some(sand.right)
      else None

    if (sand.y > lowestRock || nextSand.isEmpty) sand
    else simulateGrainOfSand(rocks, nextSand.get)
  }

  @tailrec
  def simulateLandslide(rocks: Seq[Coord] = rocksAndFloor, counter: Long = 1): Long = {
    println(counter)
    val latestGrainOfSand = simulateGrainOfSand(rocks)
    if (latestGrainOfSand.y == 0) counter
    else simulateLandslide(rocks :+ latestGrainOfSand, counter+1)
  }

  println(simulateLandslide())
}
