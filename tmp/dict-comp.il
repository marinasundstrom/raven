// IL code: dict-comp
.class private auto ansi '<Module>'
{
} // end of class <Module>

.class private auto ansi sealed Program
	extends [System.Private.CoreLib]System.Object
{
	// Methods
	.method assembly hidebysig static 
		void Main () cil managed 
	{
		// Method begins at RVA 0x2050
		// Header size: 12
		// Code size: 10 (0xa)
		.maxstack 1
		.entrypoint
		.locals init (
			[0] int32
		)

		IL_0000: nop
		IL_0001: nop
		IL_0002: call int32 Foo::GetCount()
		IL_0007: stloc.0
		IL_0008: nop
		IL_0009: ret
	} // end of method Program::Main

	.method public specialname rtspecialname 
		instance void .ctor () cil managed 
	{
		// Method begins at RVA 0x2066
		// Header size: 1
		// Code size: 7 (0x7)
		.maxstack 8

		IL_0000: ldarg.0
		IL_0001: call instance void [System.Private.CoreLib]System.Object::.ctor()
		IL_0006: ret
	} // end of method Program::.ctor

} // end of class Program

.class public auto ansi sealed Foo
	extends [System.Private.CoreLib]System.Object
{
	// Methods
	.method public hidebysig specialname rtspecialname 
		instance void .ctor () cil managed 
	{
		// Method begins at RVA 0x206e
		// Header size: 1
		// Code size: 8 (0x8)
		.maxstack 8

		IL_0000: nop
		IL_0001: ldarg.0
		IL_0002: call instance void [System.Private.CoreLib]System.Object::.ctor()
		IL_0007: ret
	} // end of method Foo::.ctor

	.method public hidebysig static 
		int32 GetCount () cil managed 
	{
		// Method begins at RVA 0x2078
		// Header size: 12
		// Code size: 112 (0x70)
		.maxstack 4
		.locals init (
			[0] class [System.Collections.Immutable]System.Collections.Immutable.ImmutableDictionary`2<string, int32>,
			[1] string,
			[2] class [System.Private.CoreLib]System.Collections.Generic.Dictionary`2<string, int32>,
			[3] string,
			[4] class [System.Private.CoreLib]System.Collections.IEnumerator
		)

		IL_0000: nop
		IL_0001: nop
		IL_0002: newobj instance void class [System.Private.CoreLib]System.Collections.Generic.Dictionary`2<string, int32>::.ctor()
		IL_0007: stloc.2
		IL_0008: ldc.i4.2
		IL_0009: newarr [System.Private.CoreLib]System.String
		IL_000e: dup
		IL_000f: ldc.i4.0
		IL_0010: ldstr "a"
		IL_0015: stelem.ref
		IL_0016: dup
		IL_0017: ldc.i4.1
		IL_0018: ldstr "bb"
		IL_001d: stelem.ref
		IL_001e: castclass [System.Private.CoreLib]System.Collections.IEnumerable
		IL_0023: callvirt instance class [System.Private.CoreLib]System.Collections.IEnumerator [System.Private.CoreLib]System.Collections.IEnumerable::GetEnumerator()
		IL_0028: stloc.s 4
		// loop start (head: IL_002a)
			IL_002a: ldloc.s 4
			IL_002c: callvirt instance bool [System.Private.CoreLib]System.Collections.IEnumerator::MoveNext()
			IL_0031: brfalse IL_0055

			IL_0036: ldloc.s 4
			IL_0038: callvirt instance object [System.Private.CoreLib]System.Collections.IEnumerator::get_Current()
			IL_003d: castclass [System.Private.CoreLib]System.String
			IL_0042: stloc.3
			IL_0043: ldloc.2
			IL_0044: ldloc.3
			IL_0045: ldloc.3
			IL_0046: callvirt instance int32 [System.Private.CoreLib]System.String::get_Length()
			IL_004b: callvirt instance void class [System.Private.CoreLib]System.Collections.Generic.Dictionary`2<string, int32>::Add(!0, !1)
			IL_0050: br IL_002a
		// end loop

		IL_0055: ldloc.2
		IL_0056: call class [System.Collections.Immutable]System.Collections.Immutable.ImmutableDictionary`2<!!0, !!1> [System.Collections.Immutable]System.Collections.Immutable.ImmutableDictionary::CreateRange<string, int32>(class [System.Private.CoreLib]System.Collections.Generic.IEnumerable`1<valuetype [System.Private.CoreLib]System.Collections.Generic.KeyValuePair`2<!!0, !!1>>)
		IL_005b: stloc.0
		IL_005c: nop
		IL_005d: ldloc.0
		IL_005e: callvirt instance int32 class [System.Collections.Immutable]System.Collections.Immutable.ImmutableDictionary`2<string, int32>::get_Count()
		IL_0063: ldloc.0
		IL_0064: ldstr "bb"
		IL_0069: callvirt instance !1 class [System.Collections.Immutable]System.Collections.Immutable.ImmutableDictionary`2<string, int32>::get_Item(!0)
		IL_006e: add
		IL_006f: ret
	} // end of method Foo::GetCount

} // end of class Foo

