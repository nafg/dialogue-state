package scripts

import scala.collection.immutable.SortedMap

import bleep.*
import bleep.model.CrossProjectName
import bleep.nosbt.InteractionService
import bleep.packaging.{CoordinatesFor, PackagedLibrary, PublishLayout, packageLibraries}
import bleep.plugin.cirelease.CiReleasePlugin
import bleep.plugin.dynver.DynVerPlugin
import bleep.plugin.pgp.PgpPlugin
import bleep.plugin.sonatype.Sonatype
import coursier.Info

object Publish extends BleepScript("Publish") {
  private val groupId = "io.github.nafg.dialogue-state"

  private def projectsToPublish(crossName: CrossProjectName): Boolean =
    crossName.name.value match {
      case "scripts" => false
      case _         => true
    }

  override def run(started: Started, commands: Commands, args: List[String]): Unit = {
    commands.compile(started.build.explodedProjects.keys.filter(projectsToPublish).toList)

    val dynVer    = new DynVerPlugin(baseDirectory = started.buildPaths.buildDir.toFile, dynverSonatypeSnapshots = true)
    val pgp       = new PgpPlugin(
      logger = started.logger,
      maybeCredentials = None,
      interactionService = InteractionService.DoesNotMaskYourPasswordExclamationOneOne
    )
    val sonatype  = new Sonatype(
      logger = started.logger,
      sonatypeBundleDirectory = started.buildPaths.dotBleepDir / "sonatype-bundle",
      sonatypeProfileName = "io.github.nafg",
      bundleName = "dialogue-state",
      version = dynVer.version,
      sonatypeCredentialHost = Sonatype.sonatypeLegacy
    )
    val ciRelease = new CiReleasePlugin(started.logger, sonatype, dynVer, pgp)

    started.logger.info(dynVer.version)

    val info = Info(
      description = "Define a Twilio app in terms of a sane call tree",
      homePage = "https://github.com/nafg/dialogue-state/",
      developers = List(Info.Developer("nafg", "Naftoli Gugenheim", "https://github.com/nafg")),
      publication = None,
      scm = CiReleasePlugin.inferScmInfo,
      licenseInfo = List(
        Info.License(
          "Apache License 2.0",
          Some("https://spdx.org/licenses/Apache-2.0.html"),
          distribution = Some("repo"),
          comments = None
        )
      )
    )

    val packagedLibraries: SortedMap[CrossProjectName, PackagedLibrary] =
      packageLibraries(
        started,
        coordinatesFor = CoordinatesFor.Default(groupId = groupId, version = dynVer.version),
        shouldInclude = projectsToPublish,
        publishLayout = PublishLayout.Maven(info)
      )

    val files: Map[RelPath, Array[Byte]] =
      packagedLibraries.flatMap { case (_, PackagedLibrary(_, files)) => files.all }

    files.foreach { case (path, bytes) =>
      started.logger.withContext(path)(using _.asString).withContext(bytes.length).debug("will publish")
    }

    ciRelease.ciRelease(files)
  }
}
