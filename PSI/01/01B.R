przyznaj_nagrode=function(){
  rzut=sample(1:6,1)
  if(rzut==6){
    return("Super bonus!")
  }else if(rzut>3){
    return("Nagroda standardowa")
  }else{
    return("Brak nagrody...")
  }
}
przyznaj_nagrode()