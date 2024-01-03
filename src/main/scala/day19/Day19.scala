package fr.gplassard.adventofcode
package day19

object Day19 {
  case class PartRanges(x: Set[Int], m: Set[Int], a: Set[Int], s: Set[Int]) {
    def hasEmptyRange(): Boolean =
      x.isEmpty || m.isEmpty || a.isEmpty || s.isEmpty
    def combinations(): Long =
      x.size.toLong * m.size.toLong * a.size.toLong * s.size.toLong
    def filter(attribute: String, predicate: Int => Boolean): PartRanges =
      attribute match
        case "x" => copy(x = x.filter(predicate))
        case "m" => copy(m = m.filter(predicate))
        case "a" => copy(a = a.filter(predicate))
        case "s" => copy(s = s.filter(predicate))
  }
  object Workflow {
    def parse(line: String): Workflow = {
      val name = line.takeWhile(_ != '{')
      val rules = line
        .dropWhile(_ != '{')
        .drop(1)
        .takeWhile(_ != '}')
        .split(",")
        .map(Rule.parse)
        .toList
      Workflow(name, rules)
    }
  }
  case class Workflow(name: String, rules: List[Rule]) {
    def process(part: Part): RuleOutcome =
      rules.find(r => r.condition.accepts(part)).get.outcome
  }
  object Rule {
    def parse(s: String): Rule = {
      val outcome =
        if (s.endsWith("A")) Accepted
        else if (s.endsWith("R")) Rejected
        else SendTo(s.split(":").last)

      if (s.contains(">"))
        Rule(
          GreaterThan(
            s.split(">")(0),
            Integer.parseInt(s.split(">")(1).split(":")(0))
          ),
          outcome
        )
      else if (s.contains("<"))
        Rule(
          LesserThan(
            s.split("<")(0),
            Integer.parseInt(s.split("<")(1).split(":")(0))
          ),
          outcome
        )
      else Rule(Default, outcome)
    }
  }
  case class Rule(condition: Condition, outcome: RuleOutcome) {
    def intersect(partRange: PartRanges): PartRanges =
      condition.intersect(partRange)
    def subtract(partRange: PartRanges): PartRanges =
      condition.subtract(partRange)
  }

  trait Condition:
    def accepts(part: Part): Boolean
    def intersect(partRange: PartRanges): PartRanges
    def subtract(partRange: PartRanges): PartRanges
  case class GreaterThan(attributeName: String, value: Int) extends Condition:
    override def accepts(part: Part): Boolean =
      part.attributes.find(_.name == attributeName).get.value > value
    override def intersect(partRange: PartRanges): PartRanges =
      partRange.filter(attributeName, _ > value)
    override def subtract(partRange: PartRanges): PartRanges =
      partRange.filter(attributeName, _ <= value)
  case class LesserThan(attributeName: String, value: Int) extends Condition:
    override def accepts(part: Part): Boolean =
      part.attributes.find(_.name == attributeName).get.value < value
    override def intersect(partRange: PartRanges): PartRanges =
      partRange.filter(attributeName, _ < value)
    override def subtract(partRange: PartRanges): PartRanges =
      partRange.filter(attributeName, _ >= value)

  object Default extends Condition:
    override def accepts(part: Part): Boolean = true
    override def intersect(partRange: PartRanges): PartRanges =
      partRange.filter("x", _ => true)
    override def subtract(partRange: PartRanges): PartRanges =
      partRange.filter("x", _ => false)

  trait RuleOutcome
  case object Rejected extends RuleOutcome
  case object Accepted extends RuleOutcome
  case class SendTo(workflow: String) extends RuleOutcome

  object Attribute {
    def parse(s: String): Attribute =
      Attribute(s.split("=")(0), Integer.parseInt(s.split("=")(1)))
  }
  case class Attribute(name: String, value: Int)
  object Part {
    def parse(line: String): Part = Part(
      line
        .replace("{", "")
        .replace("}", "")
        .split(",")
        .map(Attribute.parse)
        .toList
    )
  }
  case class Part(attributes: List[Attribute])

  def part1(lines: List[String]): Int = {
    val workflows = lines
      .takeWhile(_.nonEmpty)
      .map(Workflow.parse)
      .map(w => w.name -> w)
      .toMap
    val parts = lines.dropWhile(_.nonEmpty).drop(1).map(Part.parse)
    parts
      .filter(p => isAccepted(p, workflows("in"), workflows))
      .map(_.attributes.map(_.value).sum)
      .sum
  }

  def isAccepted(
      part: Part,
      currentWorkflow: Workflow,
      workflows: Map[String, Workflow]
  ): Boolean = {
    var currentOutcome = currentWorkflow.process(part)
    while (currentOutcome != Accepted && currentOutcome != Rejected) {
      val sendTo = currentOutcome.asInstanceOf[SendTo]
      currentOutcome = workflows(sendTo.workflow).process(part)
    }
    currentOutcome == Accepted
  }

  def part2(lines: List[String]): Long = {
    val workflows = lines
      .takeWhile(_.nonEmpty)
      .map(Workflow.parse)
      .map(w => w.name -> w)
      .toMap
    val startRange = PartRanges(
      Set.from(1 to 4000),
      Set.from(1 to 4000),
      Set.from(1 to 4000),
      Set.from(1 to 4000)
    )
    walkdown(workflows("in"), startRange, workflows)
  }

  def walkdown(
      workflow: Workflow,
      partRanges: PartRanges,
      workflows: Map[String, Workflow]
  ): Long = {
    if (partRanges.hasEmptyRange()) return 0
    workflow.rules
      .foldLeft((partRanges, 0L))((acc, rule) => {
        val (range, acceptedCount) = acc
        val (remainingRange, accepted) = walkdown(rule, range, workflows)
        (remainingRange, acceptedCount + accepted)
      })
      ._2
  }

  def walkdown(
      rule: Rule,
      partRanges: PartRanges,
      workflows: Map[String, Workflow]
  ): (PartRanges, Long) = {
    if (partRanges.hasEmptyRange()) return (partRanges, 0L)

    rule.outcome match {
      case SendTo(workflow) =>
        (
          rule.subtract(partRanges),
          walkdown(workflows(workflow), rule.intersect(partRanges), workflows)
        )
      case Accepted =>
        (rule.subtract(partRanges), rule.intersect(partRanges).combinations())
      case Rejected => (rule.subtract(partRanges), 0L)
    }
  }
}
