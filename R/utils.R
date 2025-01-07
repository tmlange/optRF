round_rec_helper = function(round.recommendation = c("thousand","hundred","ten","none")){

  round.recommendation = match.arg(round.recommendation)

  switch(
    round.recommendation,
    none = 0,
    ten = -1,
    hundred = -2,
    thousand = -3
  )
}

TwoPLmodel = function(vec, p1, p2){
  1 / (1+(p1/vec)^p2)
}

TwoPLmodel.inv = function(vec, p1, p2){
  p1/((1/vec)-1)^p2
}

estimate_runtime = function(vec, p1, p2){
  p1 + vec*p2
}
