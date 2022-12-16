import scala.annotation.tailrec

case class Vertex(
                   vertexId: Int,
                   character: Char,
                   isVisited: Boolean = false,
                   distance: Long = 0
                 )

case class Edge(
                 sourceId: Int,
                 destinationId: Int
               )

object DayTwelve extends App {


//  val input = "SabqponmabcryxxlaccszExkacctuvwjabdefghi"
//  val rowLength = 8
    val input = "abaacccccccccccccaaaaaaaccccccccccccccccccccccccccccccccccaaaaaaabaaccccccccccccccaaaaaaaaaaccccccccccccccccccccccccccccccccaaaaabaaaaacccccccccaaaaaaaaaaaaccccccccccccccccccccccccccccccccaaaaabaaaaaccccccccaaaaaaaaaaaaaacccccccccccccccccdcccccccccccccaaaaabaaaccccccccccaaaaaaaaccacacccccccccccccccccdddcccccccccccaaaaaabaaacccccccccaaaaaaaaaaccaaccccccccccccciiiiddddcccccccccccacccabcaaaccccccccaaaaaaaaaaaaaaccccccccccciiiiiijddddccccccccccccccabccaaccccccccaccaaaaaaaaaaaacccccccccciiiiiijjddddccccaacccccccabccccccccccccccaaacaaaaaaaaaaccccccciiiiippijjjddddccaaacccccccabccccccccccccccaacccccaaaaaaacccccciiiippppppjjjdddddaaaaaaccccabccccccccccccccccccccaaaaaaccccccckiiippppppqqjjjdddeeeaaaaccccabccccccccccccccccccccaaaaaaccccckkkiippppuupqqjjjjdeeeeeaacccccabccccccccccccccccccccccccaaccckkkkkkipppuuuuqqqjjjjjeeeeeacccccabccccccccccccccccccccccccccckkkkkkoppppuuuuuvqqqjjjjjkeeeecccccabcccccccccccccccccccccccccckkkkooooppppuuxuvvqqqqqqjkkkeeeeccccabccaaccaccccccccccccccccccckkkoooooopuuuuxyvvvqqqqqqkkkkeeeccccabccaaaaacccccaaccccccccccckkkoooouuuuuuuxxyyvvvvqqqqqkkkkeeccccabcaaaaacccccaaaacccccccccckkkooouuuuxxxuxxyyvvvvvvvqqqkkkeeecccabcaaaaaaaaaaaaacccccccccccjjjooottuxxxxxxxyyyyyvvvvrrrkkkeeccccabcccaaaacaaaaaaaaacaaccccccjjoootttxxxxxxxyyyyyyvvvrrkkkfffccccSbccaacccccaaaaaaaaaaaccccccjjjooottxxxxEzzzyyyyvvvrrrkkkfffccccabcccccccccaaaaaaaaaaaccccccjjjooootttxxxyyyyyvvvvrrrkkkfffcccccabcaacccccaaaaaaaaaaaccccccccjjjooottttxxyyyyywwvrrrrkkkfffcccccabaaacccccaaaaaaaaaaaaaacccccjjjjonnttxxyyyyyywwwrrlllkfffccccccabaaaaaaaaaaacaaaaaaaaaaccccccjjjnnnttxxyywwyyywwrrlllffffccccccabaaaaaaaaaaaaaaaaaaaaaaccccccjjjnntttxxwwwwwywwwrrlllfffcccccccabaaccaaaaaaaaaaaaaaacccccccccjjjnntttxwwwsswwwwwrrlllfffcccccccabaacccaaaaaaaacccaaacccccccccjjinnttttwwsssswwwsrrlllgffaccccccabccccaaaaaaccccccaaaccccccccciiinnntttsssssssssssrlllggaaccccccabccccaaaaaaaccccccccccaaccccciiinnntttsssmmssssssrlllggaaccccccabccccaacaaaacccccccaacaaaccccciinnnnnnmmmmmmmsssslllgggaaaaccccabccccccccaaacccccccaaaaacccccciiinnnnnmmmmmmmmmmllllgggaaaaccccabaaaccccccccccccccccaaaaaacccciiiinnnmmmhhhmmmmmlllgggaaaacccccabaaaaacccccccccccaaaaaaaaaccccciiiiiiihhhhhhhhmmlgggggaaaccccccabaaaaaccccaaccccaaaaaaacaacccccciiiiihhhhhhhhhhggggggcaaaccccccabaaaaccccaaaccccaaaacaaaaacccccccciiihhaaaaahhhhggggcccccccccccabaaaaaaacaaacccccaaaaaaaaaccccccccccccccaaaacccccccccccccccccaaabaacaaaaaaaaaaaccaaaaaaaaccccccccccccccccaaaccccccccccccccccaaaabcccccaaaaaaaaacccaaaaaaaccccccccccccccccaacccccccccccccccccaaaabccccccaaaaaaaaaaaaaaaaacccccccccccccccccaaacccccccccccccaaaaaaabcccccaaaaaaaaaaaaaaaaaaaaaccccccccccccccccccccccccccccccaaaaaa"
    val rowLength = 64

  val maxCharacters = input.length
  val verticesSeq = input.toList.zipWithIndex.map(inputChar => Vertex(inputChar._2, inputChar._1))

  val edgesSeq = verticesSeq.flatMap{vertex =>
    val currentVertex = vertex.vertexId
    val adjacentVertices = Seq(currentVertex + 1, currentVertex - 1, currentVertex + rowLength, currentVertex - rowLength)
    val realAdjacentVertices = adjacentVertices.filter(vertex => vertex >= 0 && vertex < maxCharacters)
    val adjacentEdges = realAdjacentVertices.map(adj => Edge(currentVertex, adj))
    adjacentEdges.filter(edge => {
      val sourceChar = verticesSeq(edge.sourceId.toInt).character
      val destinationChar = verticesSeq(edge.destinationId.toInt).character
      destinationChar != 'E' && (
        destinationChar <= sourceChar ||
          destinationChar.toInt == sourceChar.toInt + 1 ||
          (sourceChar == 'S' && (destinationChar == 'a' || destinationChar == 'b'))
        ) || sourceChar == 'y' || sourceChar == 'z'
    })
  }

  val start = verticesSeq.filter(_.character == 'S').head.vertexId
  val end = verticesSeq.filter(_.character == 'E').head.vertexId

  @tailrec
  def journeyThroughGraph(verticesSeq: Seq[Vertex], currentVertices: Set[Int], distance: Int): Int = {
    val newDistance = distance+1
    val adjacentVertices = currentVertices.flatMap(currentVertex => edgesSeq.filter(_.sourceId == currentVertex).map(_.destinationId))
    val newVertices = verticesSeq.map(vertex => if(adjacentVertices.contains(vertex.vertexId) && !vertex.isVisited) vertex.copy(isVisited=true, distance=newDistance) else vertex)
    if (newVertices(end).isVisited) newDistance
    else journeyThroughGraph(newVertices, adjacentVertices, newDistance)
  }

 println(journeyThroughGraph(verticesSeq, Set(start), 0))

  val possibleStarts = (0 until (maxCharacters/rowLength)).map(_*rowLength)

  println(journeyThroughGraph(verticesSeq, possibleStarts.toSet, 0))
}
