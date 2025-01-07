round.rec_helper = function(round.recommendation = c("thousand","hundred","ten","none")){

  round.recommendation = match.arg(round.recommendation)

  switch(
    round.recommendation,
    none = 0,
    ten = -1,
    hundred = -2,
    thousand = -3
  )
}
