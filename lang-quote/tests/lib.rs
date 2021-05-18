#[macro_use]
extern crate glsl_lang_quote;

use glsl_lang::ast;

#[test]
fn void_main_empty() {
    let _ = glsl! {void main() {}};
}

#[test]
fn fn_returns_int() {
    let _ = glsl! {
      int test() {
        return 3.;
      }
    };
}

#[test]
fn simple_struct() {
    let _ = glsl! {
      struct V {
        vec4 p;
        vec2 uv;
      };

      struct F {
        vec4 color;
      };
    };
}

#[test]
fn struct_several_ident_per_field() {
    let _ = glsl! {
      struct S {
        float a, b, c;
      };
    };
}

#[test]
fn struct_with_identifiers() {
    let _ = glsl! {
      struct S {
        float a, b, c;
      } foo, bar, zoo;
    };
}

#[test]
fn struct_with_arrayed_identifiers() {
    let _ = glsl! {
      struct S {
        float a, b, c;
      } foo[3], bar[12], zoo[];
    };
}

#[test]
fn typed_return() {
    let _ = glsl! {
      float foo() {
      }
    };
}

#[test]
fn dot_expr() {
    let _ = glsl! {
      void main() {
        float x = foo.xyz;
        float y = 1.;
        //float z = .3;
      }
    };
}

#[test]
fn quote_ident() {
    let ident: ast::Identifier = ast::IdentifierData("main".into()).into();

    let tu = glsl! {
        void #(ident)() {
        }
    };

    match &*(tu.0)[0] {
        ast::ExternalDeclarationData::FunctionDefinition(fndef) => {
            assert_eq!(fndef.prototype.name.0, "main");
        }
        _ => panic!("unexpected syntax node"),
    }
}

#[test]
fn type_qualifier() {
    let _ = glsl! {
        highp float x;
    };
}

#[test]
fn layout_qualifier() {
    let _ = glsl! {
        layout(std140) struct Q { float f; };
    };
}
