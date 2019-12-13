; ModuleID = './examples/fib.mlg'
source_filename = "./examples/fib.mlg"

declare i8* @GC_malloc(i64)

define {}* @fib_loop.31(i64, i64) {
  %3 = call i64 @fib.21(i64 %1)
  %4 = call {}* @print_int.7(i64 %3)
  %5 = call i8* @GC_malloc(i64 0)
  %6 = bitcast i8* %5 to {}*
  %7 = call {}* @newline.8({}* %6)
  %8 = icmp sle i64 %0, %1
  %9 = alloca {}*
  br i1 %8, label %then_0, label %else_0

then_0:                                           ; preds = %2
  %10 = call i8* @GC_malloc(i64 0)
  %11 = bitcast i8* %10 to {}*
  store {}* %11, {}** %9
  br label %endif_0

else_0:                                           ; preds = %2
  %12 = add i64 %1, 1
  %13 = call {}* @fib_loop.31(i64 %0, i64 %12)
  store {}* %13, {}** %9
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %14 = load {}*, {}** %9
  ret {}* %14
}

define i64 @fib.21(i64) {
  %2 = icmp sle i64 %0, 1
  %3 = alloca i64
  br i1 %2, label %then_0, label %else_0

then_0:                                           ; preds = %1
  store i64 1, i64* %3
  br label %endif_0

else_0:                                           ; preds = %1
  %4 = sub i64 %0, 1
  %5 = call i64 @fib.21(i64 %4)
  %6 = sub i64 %0, 2
  %7 = call i64 @fib.21(i64 %6)
  %8 = add i64 %5, %7
  store i64 %8, i64* %3
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %9 = load i64, i64* %3
  ret i64 %9
}

declare {}* @newline({}*)

define {}* @newline.8({}*) {
  %2 = call {}* @newline({}* %0)
  ret {}* %2
}

declare {}* @print_int(i64)

define {}* @print_int.7(i64) {
  %2 = call {}* @print_int(i64 %0)
  ret {}* %2
}

define i32 @main() {
  %1 = call {}* @fib_loop.31(i64 30, i64 0)
  ret i32 0
}