direction :: (Integral x) => x -> String
direction x
  | x == 0 = "forward"
  | x == 1 = "right"
  | x == 2 = "down"
  | x == 3 = "left"

-- where
msg :: Integral x => x -> String
msg x
  | x < thresh = "cold"
  | x > thresh = "hot"
  | x == thresh = "got it! congradulations!!"
  where thresh = 10