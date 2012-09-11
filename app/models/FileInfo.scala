package models;

import scala.util.parsing.input._
import scala.util.parsing.combinator._
import scala.util.matching._
import java.math._
import org.joda.time._
import org.joda.time.format._
import play._

import reactivemongo.api._
import reactivemongo.bson._
import reactivemongo.bson.handlers._

case class FileInfo( groupId: String, artifactId: String, version: Option[String], name: Option[String], isSnapshot: Boolean, isMetadata: Boolean, isFolder : Boolean )

object FileInfo{
    val DefaultRegex = """((/[^/]+?)*/)([^/]+)/([^/]+)/([^/]+)""".r
    val SnapShotRegex = """((/[^/]+?)*/)([^/]+)/([^/]+-SNAPSHOT)/([^/]+)""".r
    val MetadataRegex = """((/[^/]+?)*/)([^/]+)/(maven.metadata.xml)""".r
    val FolderRegex = """((/[^/]+?)*/)([^/]+)/""".r

    def cleanGroupId( groupId: String ) = {
        groupId.drop(1).replaceAll("/",".")
    }

    def apply( str: String ): Option[FileInfo] = {
        str match {
            case s if( s.matches( FolderRegex.toString ) ) => {
                val MetadataRegex(groupId, _, artifactId, name) = s
                Option(FileInfo( cleanGroupId(groupId), artifactId, None, None, false, false, true))
            }
            case s if( s.matches( MetadataRegex.toString ) ) => {
                val MetadataRegex(groupId, _, artifactId, name) = s
                Option(FileInfo( cleanGroupId(groupId), artifactId, None, Option(name), false, true, false))
            }
            case s if( s.matches( SnapShotRegex.toString ) ) => {
                val SnapShotRegex(groupId, _, artifactId, version, name) = s
                Option(FileInfo( cleanGroupId(groupId), artifactId, Option(version), Option(name), true, false, false))
            }
            case s if( s.matches( DefaultRegex.toString ) ) => {
                val DefaultRegex(groupId, _, artifactId, version, name) = s
                Option(FileInfo( cleanGroupId(groupId), artifactId, Option(version), Option(name), false, false, false))
            }
            case _ => None
        }
    }

    implicit object FileInfoBSONWriter extends BSONWriter[FileInfo] {
        def toBSON( fi: FileInfo ): AppendableBSONDocument = {
            val bson = new AppendableBSONDocument() += (
              "groupId" -> BSONString(fi.groupId),
              "artifactId" -> BSONString(fi.artifactId),
              "isSnapshot" -> BSONBoolean(fi.isSnapshot),
              "isMetadata" -> BSONBoolean(fi.isMetadata),
              "isFolder" -> BSONBoolean(fi.isMetadata)
            )
            if( fi.version.isDefined )
                bson += "version" -> BSONString(fi.version.get)
            if( fi.version.isDefined )
                bson += "name" -> BSONString(fi.name.get)
            bson
        }
    }
}