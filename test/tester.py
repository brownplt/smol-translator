import subprocess
import glob


def run_js_file(test):
    command = [
        "node",
        test,
        "2>&1"
    ]
    actual_result = subprocess.run(
        command, stdout=subprocess.PIPE).stdout.decode('utf-8')
    actual_result = actual_result.split("\n")
    actual_result = " ".join(actual_result)
    return actual_result


suffix = ".smol.js"
i = 0
for test in glob.glob("./test/test_cases/**/*{}".format(suffix)):
    i = i + 1
    if i > 10:
        break
    program = open(test).read()
    wished_results = "{}.txt.js".format(test[:-len(suffix)])
    wished_results = open(wished_results).read()
    actual_results = run_js_file(test)
    if wished_results == actual_results:
        print("PASSed {}".format(test))
    else:
        print("FAILed {}".format(test))
        print("Program:")
        print(program)
        print("Wished:")
        print(wished_results)
        print("Actual:")
        print(actual_results)
        print("----------")
        exit(-1)
