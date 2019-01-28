module ExercisesChapter2 where

waxOn = x * 5
  where x = y ^ 2
        y = z + 8
        z = 7

triple x = x * 3

-- Original waxOff
--waxOff x = triple x

-- Dave's waxOff

waxOff x = intermediate * intermediate
  where intermediate = triple x

