module Lang where


infixr 0 :->:
infixr 2 :/:
infixr 3 :&:

data R s = R s :&: R s
         | None
         | R s :/: R s
         | Fail
         | R s :->: R s
         | R s
         | Unlimited (R s)
         | Optional (R s)


data I =
