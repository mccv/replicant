package org.mccv.replicant

import java.lang.reflect.{Constructor, Field, Member, Method, Modifier}

/**
 * Various implicits for converting classes to inspectables
 */
object Replicant {
  implicit def toInspectable(c: Class[_]) = {
    new Inspectable(c)
  }
  implicit def toInspectable(a: AnyRef) = {
    new Inspectable(a.getClass)
  }
  /**
   * simple pass all filter for use in default constructors
   */
  def all[T <: Member](m: T) = true
}

/**
 * Common utilities to use in Methods, Members and Fields inspectors
 */
trait InspectableBase {
  /** ANSI terminal code for blue text */
  val BLUE = "\033[0;34m"
  /** ANSI terminal code for black text */
  val BLACK = "\033[0;30m"
  /** ANSI terminal code for green text */
  val GREEN = "\033[0;32m"
  /** ANSI terminal code for white text */
  val WHITE = "\033[0;29m"

  def clazz(): Class[_]
  val s = clazz.getSuperclass()

  /**
   * convert a potentially null super class to an option
   */
  val si = if (s != null) {
    Some(s)
  } else {
    None
  }

  /**
   * convert Java array names to readable form
   */
  def decodeClassName(name: String) = {
    name match {
      case "[B" => "byte[]"
      case "[S" => "short[]"
      case "[I" => "int[]"
      case "[J" => "long[]"
      case "[F" => "float[]"
      case "[D" => "double[]"
      case "[Z" => "boolean[]"
      case "[C" => "char[]"
      case n => if(n.startsWith("[L")) {
        n.substring(2, n.length - 1) + "[]"
      } else {
        n
      }
    }
  }

  /**
   * conversions for Scala name mangling.
   * Almost certainly incomplete.
   */
  val mangles = List(("$eq", "="),
                     ("$greater", ">"),
                     ("$less", "<"),
                     ("$bang", "!"),
                     ("$colon", ":"),
                     ("$minus", "-"),
                     ("$plus", "+"),
                     ("$bslash", "\\"),
                     ("$div", "/")
                   )
  /**
   * a terribly slow unmangler
   */
  def unmangle(name: String) = {
    mangles.foldLeft(name){ case(acc, (from, to)) => {
      acc.replace(from, to)
    }}
  }

  /**
   * given a modifier bitmask, construct a string representation.
   * Currently adds some symbol indicating a modifer is set, or
   * a blank if modifer is unset.
   */
  def inspectModifiers(mods: Int) = {
    val sb = new StringBuilder()
    sb.append(printif("A", Modifier.isAbstract(mods)))
    sb.append(printif("F", Modifier.isFinal(mods)))
    sb.append(printif("I", Modifier.isInterface(mods)))
    sb.append(printif("N", Modifier.isNative(mods)))
    sb.append(printif("-", Modifier.isPrivate(mods)))
    sb.append(printif("*", Modifier.isProtected(mods)))
    sb.append(printif("+", Modifier.isPublic(mods)))
    sb.append(printif("S", Modifier.isStatic(mods)))
    sb.append(printif("s", Modifier.isStrict(mods)))
    sb.append(printif("|", Modifier.isSynchronized(mods)))
    sb.append(printif("T", Modifier.isTransient(mods)))
    sb.append(printif("V", Modifier.isVolatile(mods)))
    sb.toString
  }

  def printif(s: String, f: => Boolean) = {
    if (f) {
      s
    } else {
      " "
    }
  }
}

/**
 * Provide common filters for all method/contructor/field inspectors.
 * The parameterization here is ugly, but necessary to allow filter to
 * return a concrete implementing type.
 */
trait Members[T <: Member, U] extends InspectableBase {
  // the abstract bit
  /** tell us what class you're inspecting */
  def clazz: Class[_]
  /** provide a filter method that generates a new (filtered) version of yourself */
  def filter(clazz: Class[_], f: T => Boolean): U

  // concrete methods
  /** utility to compose two filter methods */
  def compose(f1: T => Boolean, f2: T => Boolean): (T => Boolean) = {x: T => f1(x) && f2(x)}

  // general filters
  def filter(f: T => Boolean): U = filter(clazz, f)
  /** abstract members */
  def abs = filter(clazz, (x => Modifier.isAbstract(x.getModifiers())))
  /** final members */
  def fin = filter(clazz, (x => Modifier.isFinal(x.getModifiers())))
  /** members that are interfaces */
  def inter = filter(clazz, (x => Modifier.isInterface(x.getModifiers())))
  /** members that are native */
  def nat = filter(clazz, (x => Modifier.isNative(x.getModifiers())))
  /** protected members */
  def prot = filter(clazz, (x => Modifier.isProtected(x.getModifiers())))
  /** public members */
  def pub = filter(clazz, (x => Modifier.isPublic(x.getModifiers())))
  /** private members */
  def priv = filter(clazz, (x => Modifier.isPrivate(x.getModifiers())))
  /** static members */
  def stat = filter(clazz, (x => Modifier.isStatic(x.getModifiers())))
  /** strict members */
  def strict = filter(clazz, (x => Modifier.isStrict(x.getModifiers())))
  /** synchronized members */
  def sync = filter(clazz, (x => Modifier.isSynchronized(x.getModifiers())))
  /** transient members */
  def trans = filter(clazz, (x => Modifier.isTransient(x.getModifiers())))
  /** volatile members */
  def vol = filter(clazz, (x => Modifier.isVolatile(x.getModifiers())))
  /** all members containing the specified string in their name (case insensitive) */
  def /(find: String) = filter(clazz, (x => unmangle(x.getName).toLowerCase.contains(find.toLowerCase)))
  /** all members whose name matches the specified string (regex) */
  def ~(find: String) = filter(clazz, (x => unmangle(x.getName).matches(find)))
  /** all members whose declaring class's canonical name contains the specified string */
  def from(pkg: String) = filter(clazz, (x => unmangle(x.getDeclaringClass().getCanonicalName()).toLowerCase.contains(pkg.toLowerCase)))
}

/**
 * Inspector for methods on a class.
 * The class also takes a filter method that restricts the methods of interest
 */
class Methods(override val clazz: Class[_], f: Method => Boolean) extends Members[Method, Methods] {
  def this(clazz: Class[_]) = this(clazz, Replicant.all)
  def members = clazz.getDeclaredMethods
  def filteredMethods = members.filter(m => f(m))

  /**
   * a method-specific filter. Returns true if a method takes arguments matching the passed string.
   * The match is done against the formatted args list, e.g. "int" would match a method signature of
   * "int, char, char[]" and "char, int, char[]"
   */
  def takes(str: String) = new Methods(clazz, compose((x => this.inspectParameters(x).contains(str)), f))

  /** a Methods inspector for our superclass */
  val siMethods = si.map(sup => new Methods(sup, f))
  /** Methods inspectors for all our interfaces */
  val intMethods = clazz.getInterfaces.map(inter => new Methods(inter, f))

  override def filter(clazz: Class[_], fNew: Method => Boolean): Methods = {
    new Methods(clazz, compose(f, fNew))
  }

  def inspectMethods(): String = {
    inspectMethods(f)
  }

  /**
   * Construct a string representation of all methods that pass our filter
   */
  def inspectMethods(fLoc: Method => Boolean): String = {
    val sb = new StringBuilder()
    if (filteredMethods.size > 0) {
      sb.append(GREEN).append("=== methods from ").append(clazz.getName).append("===").append(WHITE).append("\n")
      filteredMethods.foreach(method => {
        sb.append(inspectMethod(method))
        sb.append(inspectParameters(method))
        sb.append(": ").append(decodeClassName(method.getReturnType.getName))
        sb.append("\n")
      })
    }
    siMethods.foreach(cl => sb.append(cl.inspectMethods(fLoc)))
    intMethods.foreach(cl => sb.append(cl.inspectMethods(fLoc)))
    sb.toString
  }

  def inspectMethod(m: Method) = {
    val sb = new StringBuilder()
    sb.append(inspectModifiers(m.getModifiers())).append("\t")
    sb.append(unmangle(m.getName))
    sb.toString
  }

  def inspectParameters(m: Method) = {
    val params = m.getParameterTypes.map(param => decodeClassName(param.getName)).mkString(", ")
    if (m.isVarArgs) {
      "(" + params.substring(0, params.length - 2) + "*)"
    } else {
      "(" + params + ")"
    }
  }

  override def toString() = {
    inspectMethods()
  }
}

/**
 * Very similar to Methods, but for Constructors.
 * This class is simpler, as we don't track superclass constructors.
 */
class Constructors(override val clazz: Class[_], f: Constructor[_] => Boolean) extends Members[Constructor[_], Constructors] {
  def this(clazz: Class[_]) = this(clazz, Replicant.all)
  def members = clazz.getDeclaredConstructors
  def filteredConstructors = members.filter(c => f(c))
  def takes(str: String) = new Constructors(clazz, compose((x => this.inspectParameters(x).contains(str)), f))

  override def filter(clazz: Class[_], fNew: Constructor[_] => Boolean): Constructors = {
    new Constructors(clazz, compose(f, fNew))
  }

  def inspectConstructors(): String = {
    val sb = new StringBuilder()
    sb.append(GREEN).append("=== constructors for ").append(clazz.getName).append("===").append(WHITE).append("\n")
    filteredConstructors.foreach(cons => {
      sb.append(inspectConstructor(cons))
      sb.append(inspectParameters(cons))
      sb.append("\n")
    })
    sb.toString
  }

  def inspectConstructor(c: Member) = {
    val sb = new StringBuilder()
    sb.append(inspectModifiers(c.getModifiers())).append("\t")
    sb.append(unmangle(c.getName))
    sb.toString
  }

  def inspectParameters(c: Constructor[_]) = {
    c.getParameterTypes.map(param => decodeClassName(param.getName)).mkString("(", ", ", ")")
  }

  override def toString = inspectConstructors
}

/**
 * Very similar to Methods, but for Constructors.
 * This class is simpler, as we don't track superclass constructors.
 */
class Fields(override val clazz: Class[_], f: Field => Boolean) extends Members[Field, Fields] {
  def this(clazz: Class[_]) = this(clazz, Replicant.all)
  def members = clazz.getDeclaredFields()
  def filteredFields = members.filter(field => f(field))

  /** a Fields inspector for our superclass */
  val siFields = si.map(sup => new Fields(sup, f))
  /** Fields inspectors for all our interfaces */
  val intFields = clazz.getInterfaces.map(inter => new Fields(inter, f))

  override def filter(clazz: Class[_], fNew: Field => Boolean): Fields = {
    new Fields(clazz, compose(f, fNew))
  }

  def inspectFields(): String = {
    inspectFields(f)
  }

  def inspectFields(fLoc: Field => Boolean): String = {
    val sb = new StringBuilder()
    val banner = new StringBuilder()
    banner.append(GREEN).append("=== fields from ").append(clazz.getName).append("===").append(WHITE).append("\n")
    filteredFields.foreach(field => {
      sb.append(inspectField(field))
      sb.append("\n")
    })
    siFields.foreach(cl => sb.append(cl.inspectFields(fLoc)))
    intFields.foreach(cl => sb.append(cl.inspectFields(fLoc)))
    if (sb.size > 0) {
      banner.append(sb)
      banner.toString
    } else {
      sb.toString
    }
  }

  def inspectField(c: Member) = {
    val sb = new StringBuilder()
    sb.append(inspectModifiers(c.getModifiers())).append("\t")
    sb.append(unmangle(c.getName))
    sb.toString
  }

  override def toString = inspectFields
}

class Inspectable(val clazz: Class[_]) extends InspectableBase {
  def this(a: AnyRef) = this(a.getClass)

  val m = new Methods(clazz)
  val c = new Constructors(clazz)
  val f = new Fields(clazz)
}
