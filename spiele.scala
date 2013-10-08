


case class Paar(team1: String,team2: String)
case class Erg(punkte1: Int, punkte2: Int)
case class Spiel(paarung: Paar, ergebnis: Erg)

object Aufgabe4 extends App{
  
	val spiele= Spiel(Paar("bra", "mex"), Erg(3, 0)):: Spiel(Paar("bra", "jap"), Erg(2, 1)):: Spiel(Paar("jap", "mex"), Erg(1, 2))::
		Spiel(Paar("kor", "bra"), Erg(0, 3)):: Spiel(Paar("kor", "mex"), Erg(1, 1)):: Spiel(Paar("kor", "jap"), Erg(1, 0))::Nil

	def punkte(land: String, spiele:List[Spiel]): Int =spiele match{
	  case kopf::Nil =>punkt(land,kopf)
	  case kopf::rumpf =>punkt(land,kopf)+punkte(land,rumpf)
	}
  
	
	def punkt(land:String, spiel:Spiel):Int={
	  if(spiel.paarung.team1==land){
	    if(spiel.ergebnis.punkte1>spiel.ergebnis.punkte2){3}
	    else if(spiel.ergebnis.punkte1==spiel.ergebnis.punkte2){1}
	    else{0}
	  }
	  else if(spiel.paarung.team2==land){
	    if(spiel.ergebnis.punkte1<spiel.ergebnis.punkte2){3}
	    else if(spiel.ergebnis.punkte1==spiel.ergebnis.punkte2){1}
	    else{0}
	  }
	  else{0}
	}
	
	def Teams(spiele: List[Spiel]):List[String]=spiele match{
	  
	  case List() => List()
	  case kopf::rumpf => (kopf.paarung.team1 ::kopf.paarung.team2 :: Teams(rumpf)).removeDuplicates
	  
	  
	  
	} 
	
	println(punkte("bra",spiele))
	println(Teams(spiele))
	
	
}