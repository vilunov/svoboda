package svoboda

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.whitebox

@compileTimeOnly("Cannot expand @toInitial")
class toInitial[F[_]] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro toInitial.macros.inst
}

object toInitial {
  private class macros(val c: whitebox.Context) {
    def abort(msg: String): Nothing = c.abort(c.enclosingPosition, msg)

    import c.universe._

    def inst(annottees: c.Tree*): c.Tree = {
      val moduleType: Tree = c.prefix.tree match {
        case q"new $_[${module: Ident}]()" if module != q"Nothing" =>
          c.typecheck(module, c.TYPEmode)
        case _ =>
          abort("""Failed to derive initial encoding, the type parameter must be explicitly specified, e.g.: "@toInitial[Monoid]"""")
      }
      val paramF = moduleType.tpe.typeParams match {
        case List(param) => param
        case _ => abort("F must be of kind F[_]")
      }
      val algMembers: Iterable[MethodSymbol] = moduleType.tpe.members.flatMap {
        case e: MethodSymbol if e.isAbstract && e.isPublic && (e.returnType.typeSymbol eq paramF) => Some(e)
        case e if !e.isAbstract => None
        case _ => abort("Only abstract methods returning F[T] are expected in the module")
      }
      val (traitDef, name) = annottees match {
        case Seq(traitDef @ q"sealed trait ${name: TypeName}[$typeParam]") if typeParam.tparams.isEmpty =>
          (traitDef, name)
        case _ =>
          abort("Failed to derive initial encoding, `the annotation should be applied to an empty sealed trait of kind F[_]")
      }

      val traitName = name.toTermName
      val typeV: Tree = tq"$moduleType[$name]"
      val (cases, instanceMethods) = algMembers.map { method: MethodSymbol =>
        val resultType = method.returnType.typeArgs.head
        val resultTypeF = tq"$name[$resultType]"
        val fields =
          for (i <- method.typeSignature.paramLists.flatten)
            yield q"${i.name.toTermName}: ${i.typeSignature}"
        val methodArgs =
          for (paramList <- method.paramLists)
            yield for (param <- paramList)
              yield ValDef(Modifiers.apply(), param.name.toTermName, tq"${param.typeSignature}", EmptyTree)

        val className = q"${traitName.toTermName}.${method.name.toTermName}"
        if (methodArgs.isEmpty) {
          val caseObject = q"""case object ${method.name.toTermName} extends $resultTypeF"""
          val instanceMethod = ValDef(
            mods = Modifiers(Flag.OVERRIDE, typeNames.EMPTY, List.empty),
            name = method.name.toTermName,
            tpt = resultTypeF,
            rhs = q"$className",
          )
          (caseObject, instanceMethod)
        } else {
          val caseClass = q"""final case class ${method.name.toTypeName}(..$fields) extends $resultTypeF"""
          val instanceMethod = DefDef(
            mods = Modifiers(Flag.OVERRIDE, typeNames.EMPTY, List.empty),
            name = method.name.toTermName,
            tparams = List.empty,
            vparamss = methodArgs,
            tpt = resultTypeF,
            rhs = q"$className(..$fields)",
          )
          (caseClass, instanceMethod)
        }
      }.unzip

      q"""
        $traitDef
        object $traitName {
          ..$cases

          implicit val instance: $typeV = new $typeV {
              ..$instanceMethods
          }
        }
      """
    }
  }

}
