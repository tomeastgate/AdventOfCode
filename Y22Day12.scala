package org.example

import org.apache.spark._
import org.apache.spark.graphx._
import org.apache.spark.graphx.lib.ShortestPaths
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession

object DayTwelve extends App {


//  val input = "SabqponmabcryxxlaccszExkacctuvwjabdefghi"
//  val rowLength = 8
  val input = "abaacccccccccccccaaaaaaaccccccccccccccccccccccccccccccccccaaaaaaabaaccccccccccccccaaaaaaaaaaccccccccccccccccccccccccccccccccaaaaabaaaaacccccccccaaaaaaaaaaaaccccccccccccccccccccccccccccccccaaaaabaaaaaccccccccaaaaaaaaaaaaaacccccccccccccccccdcccccccccccccaaaaabaaaccccccccccaaaaaaaaccacacccccccccccccccccdddcccccccccccaaaaaabaaacccccccccaaaaaaaaaaccaaccccccccccccciiiiddddcccccccccccacccabcaaaccccccccaaaaaaaaaaaaaaccccccccccciiiiiijddddccccccccccccccabccaaccccccccaccaaaaaaaaaaaacccccccccciiiiiijjddddccccaacccccccabccccccccccccccaaacaaaaaaaaaaccccccciiiiippijjjddddccaaacccccccabccccccccccccccaacccccaaaaaaacccccciiiippppppjjjdddddaaaaaaccccabccccccccccccccccccccaaaaaaccccccckiiippppppqqjjjdddeeeaaaaccccabccccccccccccccccccccaaaaaaccccckkkiippppuupqqjjjjdeeeeeaacccccabccccccccccccccccccccccccaaccckkkkkkipppuuuuqqqjjjjjeeeeeacccccabccccccccccccccccccccccccccckkkkkkoppppuuuuuvqqqjjjjjkeeeecccccabcccccccccccccccccccccccccckkkkooooppppuuxuvvqqqqqqjkkkeeeeccccabccaaccaccccccccccccccccccckkkoooooopuuuuxyvvvqqqqqqkkkkeeeccccabccaaaaacccccaaccccccccccckkkoooouuuuuuuxxyyvvvvqqqqqkkkkeeccccabcaaaaacccccaaaacccccccccckkkooouuuuxxxuxxyyvvvvvvvqqqkkkeeecccabcaaaaaaaaaaaaacccccccccccjjjooottuxxxxxxxyyyyyvvvvrrrkkkeeccccabcccaaaacaaaaaaaaacaaccccccjjoootttxxxxxxxyyyyyyvvvrrkkkfffccccSbccaacccccaaaaaaaaaaaccccccjjjooottxxxxEzzzyyyyvvvrrrkkkfffccccabcccccccccaaaaaaaaaaaccccccjjjooootttxxxyyyyyvvvvrrrkkkfffcccccabcaacccccaaaaaaaaaaaccccccccjjjooottttxxyyyyywwvrrrrkkkfffcccccabaaacccccaaaaaaaaaaaaaacccccjjjjonnttxxyyyyyywwwrrlllkfffccccccabaaaaaaaaaaacaaaaaaaaaaccccccjjjnnnttxxyywwyyywwrrlllffffccccccabaaaaaaaaaaaaaaaaaaaaaaccccccjjjnntttxxwwwwwywwwrrlllfffcccccccabaaccaaaaaaaaaaaaaaacccccccccjjjnntttxwwwsswwwwwrrlllfffcccccccabaacccaaaaaaaacccaaacccccccccjjinnttttwwsssswwwsrrlllgffaccccccabccccaaaaaaccccccaaaccccccccciiinnntttsssssssssssrlllggaaccccccabccccaaaaaaaccccccccccaaccccciiinnntttsssmmssssssrlllggaaccccccabccccaacaaaacccccccaacaaaccccciinnnnnnmmmmmmmsssslllgggaaaaccccabccccccccaaacccccccaaaaacccccciiinnnnnmmmmmmmmmmllllgggaaaaccccabaaaccccccccccccccccaaaaaacccciiiinnnmmmhhhmmmmmlllgggaaaacccccabaaaaacccccccccccaaaaaaaaaccccciiiiiiihhhhhhhhmmlgggggaaaccccccabaaaaaccccaaccccaaaaaaacaacccccciiiiihhhhhhhhhhggggggcaaaccccccabaaaaccccaaaccccaaaacaaaaacccccccciiihhaaaaahhhhggggcccccccccccabaaaaaaacaaacccccaaaaaaaaaccccccccccccccaaaacccccccccccccccccaaabaacaaaaaaaaaaaccaaaaaaaaccccccccccccccccaaaccccccccccccccccaaaabcccccaaaaaaaaacccaaaaaaaccccccccccccccccaacccccccccccccccccaaaabccccccaaaaaaaaaaaaaaaaacccccccccccccccccaaacccccccccccccaaaaaaabcccccaaaaaaaaaaaaaaaaaaaaaccccccccccccccccccccccccccccccaaaaaa"
  val rowLength = 64

  val maxCharacters = input.length
  val inputChar = input.toList.zipWithIndex
  val start = inputChar.filter(_._1 == 'S').head._2
  val end = inputChar.filter(_._1 == 'E').head._2

  val verticesSeq = inputChar.map(charWithIndex => (charWithIndex._2.toLong, charWithIndex._1))
  val edgesSeq = verticesSeq.flatMap{vertex =>
    val currentVertex = vertex._1
    val adjacentVertices = Seq(currentVertex + 1, currentVertex - 1, currentVertex + rowLength, currentVertex - rowLength)
    val realAdjacentVertices = adjacentVertices.filter(vertex => vertex >= 0 && vertex < maxCharacters)
    val adjacentEdges = realAdjacentVertices.map(adj => Edge(currentVertex, adj, 0))
    adjacentEdges.filter(edge => {
      inputChar(edge.dstId.toInt)._1 != 'E' && (
      inputChar(edge.dstId.toInt)._1 <= inputChar(edge.srcId.toInt)._1 ||
      inputChar(edge.dstId.toInt)._1.toInt == inputChar(edge.srcId.toInt)._1.toInt + 1 ||
      (inputChar(edge.srcId.toInt)._1 == 'S' && (inputChar(edge.dstId.toInt)._1 == 'a' || inputChar(edge.dstId.toInt)._1 == 'b'))) ||
      inputChar(edge.srcId.toInt)._1 == 'y' || inputChar(edge.srcId.toInt)._1 == 'z'
    })
  }

  val spark = SparkSession.builder.config("spark.master", "local").appName("Simple Application").getOrCreate()

  import spark.implicits._

  val vertices: RDD[(VertexId, Char)] = spark.sparkContext.parallelize(verticesSeq)

  val edges: RDD[Edge[Int]] = spark.sparkContext.parallelize(edgesSeq)

  val graph = Graph(vertices, edges)

  val shortestPathRunner = ShortestPaths.run(graph, Seq(end))

  val shortestPath = shortestPathRunner
    .vertices
    .filter({case(vId, _) => vId == start})
    .first
    ._2
    .get(end)

  println(shortestPath)

  spark.close()
}
