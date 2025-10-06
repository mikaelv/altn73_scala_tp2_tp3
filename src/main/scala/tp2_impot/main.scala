package tp2_impot


@main
def main(): Unit =
  testCelibataire()
  testCalculProgressif()
  testFoyerEnfants()

def testCelibataire(): Unit =
  val pierre = Celibataire("Pierre Dupont", 35000.0)
  println(s"${pierre.nom}: ${pierre.revenuTotal}€, ${pierre.nombreParts} parts")
// Sortie attendue: Pierre Dupont: 35000.0€, 1.0 parts

  val marie = Celibataire("Marie Martin", 40000.0)
  println(f"${marie.nom}:")
  println(f"  Quotient familial: ${marie.calculerQuotientFamilial()}%.2f€")
  println(f"  Taux effectif: ${marie.calculerTauxEffectif(6000)}%.2f%%")
  // Sortie attendue:
  // Marie Martin:
  //   Quotient familial: 40000.00€
  //   Impôt à 15%: 6000.00€
  //   Taux effectif: 15.00%

// PARTIE 4
def testCalculProgressif(): Unit =
  val testeurs = List(
    Celibataire("Paul", 8000.0), // Exonéré
    Celibataire("Sophie", 20000.0), // Tranche 11%
    Celibataire("Jean", 50000.0), // Tranche 30%
    Celibataire("Claire", 100000.0) // Tranche 41%
  )

  println("=== Calcul progressif ===")
  testeurs.foreach { foyer =>
    val impot = foyer.calculerImpotProgressif()
    val taux = foyer.calculerTauxEffectif(impot)
    println(f"${foyer.nom}: ${impot}%.2f€ (${taux}%.2f%%)")
  }
  testFoyerEnfants()

def testFoyerEnfants(): Unit =
  val celibataireSeul = Celibataire("Alice", 45000.0)
  val couple = new Couple("Bob", "Emma", 25000.0, 20000.0)
  val celibataireAvecEnfants =
    FoyerAvecEnfants(Celibataire("Carol", 45000.0), 2)
  val coupleAvecEnfants = FoyerAvecEnfants(couple, 1)
  val coupleAvec4Enfants = FoyerAvecEnfants(couple, 4)

  val foyers =
    List(celibataireSeul, couple, celibataireAvecEnfants, coupleAvecEnfants)

  println("=== Comparaison des foyers ===")
  foyers.foreach { foyer =>
    val impot = foyer.calculerImpotProgressif()
    val quotient = foyer.calculerQuotientFamilial()
    val taux = foyer.calculerTauxEffectif(impot)

    val nom = foyer.toString
    println(f"$nom:")
    println(f"  Revenu total: ${foyer.revenuTotal}%.2f€")
    println(f"  Nombre de parts: ${foyer.nombreParts}%.1f")
    println(f"  Quotient familial: ${quotient}%.2f€")
    println(f"  Impôt: ${impot}%.2f€")
    println(f"  Taux effectif: ${taux}%.2f%%")
    println()
  }
