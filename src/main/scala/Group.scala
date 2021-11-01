class Group(
    val name: String,
    val people: List[Person],
    vMap: Map[String, Vaccine],
    eSet: Set[String]
):
  val vList: List[(Person, Option[Vaccine])] =
    people.map(p => p -> vMap.get(p.number))

  val (erList, nrList): (List[Person], List[Person]) =
    vList.collect { case (p, None) => p }.partition(p => eSet(p.number))

  val nraList: List[Person] = nrList.filter(_.active)

  val rList: List[(Person, Vaccine)] =
    vList.collect { case (p, Some(v)) => p -> v }

  val ongoingList: List[Person] = rList.filter(_._2.ongoing).map(_._1)

  def total = people.length
  def notResponded = nrList.length
  def notRespondedActive = nraList.length
  def errResponded = erList.length
  def responded = rList.length
  def unvaccinated = rList.count(_._2.unvaccinated)
  def ongoing = ongoingList.length
  def completed = rList.count(_._2.completed)

  def getStat: List[Double] =
    List(
      total.toDouble,
      notResponded.toDouble,
      notRespondedActive.toDouble,
      errResponded.toDouble,
      responded.toDouble,
      unvaccinated.toDouble,
      ongoing.toDouble,
      completed.toDouble
    )
