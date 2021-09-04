use criterion::{
    criterion_group, criterion_main, measurement::Measurement, BenchmarkGroup, Criterion,
    Throughput,
};

use glsl_lang::parse::IntoParseBuilderExt;

fn parse_impl_glsl_lang<T, M>(group: &mut BenchmarkGroup<'_, M>, input: &str)
where
    T: glsl_lang::parse::Parse,
    M: Measurement,
{
    group.bench_with_input("glsl_lang", input, |b, input| {
        use glsl_lang::parse::LangParser;

        let parser = T::Parser::new();
        let opts = glsl_lang::parse::ParseOptions::new();

        b.iter(|| {
            input
                .builder::<T>()
                .opts(&opts)
                .parser(&parser)
                .parse()
                .ok();
        })
    });
}

fn parse_impl_glsl<T, M>(group: &mut BenchmarkGroup<'_, M>, input: &str)
where
    T: glsl::parser::Parse,
    M: Measurement,
{
    group.bench_with_input("glsl", input, |b, input| {
        b.iter(|| {
            T::parse(input).ok();
        })
    });
}

fn parse_impl<L, G>(c: &mut Criterion, input: &str, name: &str)
where
    L: glsl_lang::parse::Parse,
    G: glsl::parser::Parse,
{
    let mut group = c.benchmark_group(name);
    group.throughput(Throughput::Bytes(input.len() as _));

    parse_impl_glsl::<G, _>(&mut group, input);
    parse_impl_glsl_lang::<L, _>(&mut group, input);

    group.finish();
}

macro_rules! bench_file {
    ($c:ident, $name:literal) => {
        parse_impl::<glsl_lang::ast::TranslationUnit, glsl::syntax::TranslationUnit>(
            $c,
            include_str!(concat!("../../data/", $name)),
            $name,
        );
    };
}

fn parse(c: &mut Criterion) {
    parse_impl::<glsl_lang::ast::TranslationUnit, glsl::syntax::TranslationUnit>(
        c,
        "void main() { ((((((((1.0f)))))))); }",
        "nested_parens",
    );

    bench_file!(c, "140.vert");
    bench_file!(c, "300block.frag");
    bench_file!(c, "300layout.frag");
    bench_file!(c, "300link2.frag");
    bench_file!(c, "300link3.frag");
    bench_file!(c, "300link.frag");
    bench_file!(c, "300operations.frag");
    bench_file!(c, "310AofA.vert");
    bench_file!(c, "310implicitSizeArrayError.vert");
    bench_file!(c, "310.inheritMemory.frag");
    bench_file!(c, "310runtimeArray.vert");
    bench_file!(c, "320.comp");
    bench_file!(c, "320.vert");
    bench_file!(c, "400.vert");
    bench_file!(c, "410.geom");
    bench_file!(c, "410.tesc");
    bench_file!(c, "410.vert");
    bench_file!(c, "420.frag");
    bench_file!(c, "420.tese");
    bench_file!(c, "430AofA.frag");
    bench_file!(c, "435.vert");
    bench_file!(c, "450.frag");
    bench_file!(c, "450.tesc");
    bench_file!(c, "450.tese");
    bench_file!(c, "460.vert");
    bench_file!(c, "array100.frag");
    bench_file!(c, "array.frag");
    bench_file!(c, "atomic_uint.frag");
    bench_file!(c, "comment.frag");
    bench_file!(c, "compoundsuffix.vert.glsl");
    bench_file!(c, "constErrors.frag");
    bench_file!(c, "constFold.frag");
    bench_file!(c, "conversion.frag");
    bench_file!(c, "cppDeepNest.frag");
    bench_file!(c, "cppIntMinOverNegativeOne.frag");
    bench_file!(c, "cppPassMacroName.frag");
    bench_file!(c, "dce.frag");
    bench_file!(c, "decls.frag");
    bench_file!(c, "deepRvalue.frag");
    bench_file!(c, "empty2.frag");
    bench_file!(c, "empty3.frag");
    bench_file!(c, "empty.frag");
    bench_file!(c, "errors.frag");
    bench_file!(c, "es-link1.frag");
    bench_file!(c, "foo.h");
    bench_file!(c, "forLoop.frag");
    bench_file!(c, "functionSemantics.frag");
    bench_file!(c, "glsl.140.layoutOffset.error.vert");
    bench_file!(c, "glsl.430.layoutOffset.error.vert");
    bench_file!(c, "glsl.450.subgroup.frag");
    bench_file!(c, "glsl.450.subgroup.vert");
    bench_file!(c, "glsl.entryPointRename2.vert");
    bench_file!(c, "glsl.entryPointRename.vert");
    bench_file!(c, "glsl.es300.layoutOffset.error.vert");
    bench_file!(c, "glsl.es320.subgroup.frag");
    bench_file!(c, "glsl.es320.subgroup.vert");
    bench_file!(c, "glspv.esversion.vert");
    bench_file!(c, "glspv.version.frag");
    bench_file!(c, "glspv.version.vert");
    bench_file!(c, "implicitInnerAtomicUint.frag");
    bench_file!(c, "include.vert");
    bench_file!(c, "invalidSwizzle.vert");
    bench_file!(c, "iomap.crossStage.2.frag");
    bench_file!(c, "iomap.crossStage.2.vert");
    bench_file!(c, "iomap.crossStage.frag");
    bench_file!(c, "iomap.crossStage.vert");
    bench_file!(c, "iomap.crossStage.vk.frag");
    bench_file!(c, "iomap.crossStage.vk.vert");
    bench_file!(c, "link1.frag");
    bench_file!(c, "link1.vk.frag");
    bench_file!(c, "link2.frag");
    bench_file!(c, "link2.vk.frag");
    bench_file!(c, "link3.frag");
    bench_file!(c, "link.multiAnonBlocksInvalid.0.0.vert");
    bench_file!(c, "link.multiAnonBlocksInvalid.0.1.vert");
    bench_file!(c, "link.multiAnonBlocksValid.0.0.vert");
    bench_file!(c, "link.multiAnonBlocksValid.0.1.vert");
    bench_file!(c, "link.multiBlocksInvalid.0.0.vert");
    bench_file!(c, "link.multiBlocksInvalid.0.1.vert");
    bench_file!(c, "link.multiBlocksValid.1.0.vert");
    bench_file!(c, "link.multiBlocksValid.1.1.vert");
    bench_file!(c, "link.vk.differentPC.0.0.frag");
    bench_file!(c, "link.vk.differentPC.0.1.frag");
    bench_file!(c, "link.vk.differentPC.0.2.frag");
    bench_file!(c, "link.vk.differentPC.1.0.frag");
    bench_file!(c, "link.vk.differentPC.1.1.frag");
    bench_file!(c, "link.vk.differentPC.1.2.frag");
    bench_file!(c, "link.vk.inconsistentGLPerVertex.0.vert");
    bench_file!(c, "link.vk.matchingPC.0.0.frag");
    bench_file!(c, "link.vk.matchingPC.0.1.frag");
    bench_file!(c, "link.vk.matchingPC.0.2.frag");
    bench_file!(c, "link.vk.multiBlocksValid.0.0.vert");
    bench_file!(c, "link.vk.multiBlocksValid.0.1.vert");
    bench_file!(c, "link.vk.pcNamingInvalid.0.0.vert");
    bench_file!(c, "link.vk.pcNamingInvalid.0.1.vert");
    bench_file!(c, "link.vk.pcNamingValid.0.0.vert");
    bench_file!(c, "link.vk.pcNamingValid.0.1.vert");
    bench_file!(c, "mains1.frag");
    bench_file!(c, "mains2.frag");
    bench_file!(c, "mains.frag");
    bench_file!(c, "maxClipDistances.vert");
    bench_file!(c, "missingBodies.vert");
    bench_file!(c, "negativeArraySize.comp");
    bench_file!(c, "newTexture.frag");
    bench_file!(c, "noMain.vert");
    bench_file!(c, "nonVulkan.frag");
    bench_file!(c, "nosuffix");
    bench_file!(c, "nvShaderNoperspectiveInterpolation.frag");
    bench_file!(c, "Operations.frag");
    bench_file!(c, "parentBad");
    bench_file!(c, "pointCoord.frag");
    bench_file!(c, "precise_struct_block.vert");
    bench_file!(c, "precise.tesc");
    bench_file!(c, "precision.vert");
    bench_file!(c, "prepost.frag");
    bench_file!(c, "preprocessor.defined.vert");
    bench_file!(c, "preprocessor.extensions.vert");
    bench_file!(c, "preprocessor.function_macro.vert");
    bench_file!(c, "preprocessor.line.frag");
    bench_file!(c, "preprocessor.many.endif.vert");
    bench_file!(c, "preprocessor.pragma.vert");
    bench_file!(c, "recurse1.frag");
    bench_file!(c, "recurse1.vert");
    bench_file!(c, "recurse2.frag");
    bench_file!(c, "reflection.frag");
    bench_file!(c, "reflection.linked.frag");
    bench_file!(c, "reflection.linked.vert");
    bench_file!(c, "reflection.options.vert");
    bench_file!(c, "reflection.vert");
    bench_file!(c, "remap.basic.dcefunc.frag");
    bench_file!(c, "remap.basic.everything.frag");
    bench_file!(c, "remap.basic.none.frag");
    bench_file!(c, "remap.basic.strip.frag");
    bench_file!(c, "remap.if.everything.frag");
    bench_file!(c, "remap.if.none.frag");
    bench_file!(c, "remap.similar_1a.everything.frag");
    bench_file!(c, "remap.similar_1a.none.frag");
    bench_file!(c, "remap.similar_1b.everything.frag");
    bench_file!(c, "remap.similar_1b.none.frag");
    bench_file!(c, "remap.switch.everything.frag");
    bench_file!(c, "remap.switch.none.frag");
    bench_file!(c, "remap.uniformarray.everything.frag");
    bench_file!(c, "remap.uniformarray.none.frag");
    bench_file!(c, "runtimeArray.vert");
    bench_file!(c, "switch.frag");
    bench_file!(c, "terminate.frag");
    bench_file!(c, "terminate.vert");
    bench_file!(c, "textureoffset_sampler2darrayshadow.vert");
    bench_file!(c, "types.frag");
    bench_file!(c, "uint.frag");
    bench_file!(c, "uniformArray.frag");
    bench_file!(c, "versionsClean.frag");
    bench_file!(c, "vk.relaxed.changeSet.frag");
    bench_file!(c, "vk.relaxed.changeSet.vert");
    bench_file!(c, "vk.relaxed.errorcheck.frag");
    bench_file!(c, "vk.relaxed.errorcheck.vert");
    bench_file!(c, "vk.relaxed.frag");
    bench_file!(c, "vk.relaxed.link1.frag");
    bench_file!(c, "vk.relaxed.link2.frag");
    bench_file!(c, "vk.relaxed.stagelink.frag");
    bench_file!(c, "vk.relaxed.stagelink.vert");
    bench_file!(c, "vulkan.ast.vert");
    bench_file!(c, "web.array.frag");
    bench_file!(c, "web.basic.vert");
    bench_file!(c, "web.builtins.frag");
    bench_file!(c, "web.controlFlow.frag");
    bench_file!(c, "web.operations.frag");
    bench_file!(c, "web.texture.frag");
    bench_file!(c, "xfbUnsizedArray.error.vert");
}

criterion_group!(glsl, parse);
criterion_main!(glsl);
