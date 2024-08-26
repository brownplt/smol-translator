import subprocess
import glob

def run_js_file(test):
    command = [
        "node",
        test
    ]
    actual_result = subprocess.run(
        command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout = actual_result.stdout.decode('utf-8')
    stderr = actual_result.stderr.decode('utf-8')
    if stderr == "":
        actual_result = stdout
    else:
        actual_result = stdout + "\n" + "error"
    actual_result = actual_result.strip().split("\n")
    actual_result = [s for s in actual_result if s != '']
    actual_result = " ".join(s.strip() for s in actual_result)
    actual_result = actual_result.strip().split("undefined")
    actual_result = [s for s in actual_result if s != '']
    actual_result = " ".join(s.strip() for s in actual_result)
    return actual_result


def run_py_file(test):
    command = [
        "python3",
        test
    ]
    actual_result = subprocess.run(
        command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout = actual_result.stdout.decode('utf-8')
    stderr = actual_result.stderr.decode('utf-8')
    if stderr == "":
        actual_result = stdout
    else:
        actual_result = stdout + "\n" + "error"
    actual_result = actual_result.strip().split("\n")
    actual_result = [s for s in actual_result if s != '']
    actual_result = [s[:-2] if s.endswith(".0") else s for s in actual_result] # remove trailing .0
    actual_result = " ".join(s.strip() for s in actual_result)
    actual_result = actual_result.strip().split("None")
    actual_result = [s for s in actual_result if s != '']
    actual_result = " ".join(s.strip() for s in actual_result)
    return actual_result


def run_scala_file(test):
    src = open(test).read()
    prefix = "import scala.collection.mutable.Buffer; object Main {def main(args: Array[String]): Unit = {"
    suffix = "}}"
    dst = prefix + "\n" + src + "\n" + suffix
    f = open("tmp.scala", "w+")
    f.write(dst)
    f.close()
    command = [
        "scala",
        "tmp.scala"
    ]
    actual_result = subprocess.run(
        command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout = actual_result.stdout.decode('utf-8')
    stderr = actual_result.stderr.decode('utf-8')
    if stderr == "":
        actual_result = stdout
    else:
        actual_result = stdout + "\n" + "error"
        # actual_result = stdout + "\n" + stderr
    # Remove Newlines
    actual_result = actual_result.strip().split("\n")
    actual_result = [s for s in actual_result if s != '']
    actual_result = " ".join(s.strip() for s in actual_result)
    # replace ArrayBuffer with Buffer
    actual_result = actual_result.replace("ArrayBuffer", "Buffer")
    return actual_result

for test_path in ["style_tests", "test_cases"]:
    suffix = ".js"
    # i = 0
    for test in glob.glob("./test/{}/*{}".format(test_path, suffix)):
        # i = i + 1
        # if i > 10:
        #     break
        program = open(test).read()
        try:
            wished_results = "{}.js.txt".format(test[:-len(suffix)])
            wished_results = open(wished_results).read().strip()
            actual_results = run_js_file(test)
            if wished_results == actual_results:
                # print("PASSED {}".format(test))
                pass
            else:
                print("FAILED {}".format(test))
                print("Program:")
                print(program)
                print("Wished: {}".format(repr(wished_results)))
                print("Actual: {}".format(repr(actual_results)))
                print("----------")
        except FileNotFoundError as e:
            print("FAILED {}".format(test))
            print("Program:")
            print(program)
            print("No expected output.")
            print("----------")


    suffix = ".py"
    # i = 0
    for test in glob.glob("./test/{}/*{}".format(test_path, suffix)):
        # i = i + 1
        # if i > 10:
        #     break
        program = open(test).read()
        try:
            wished_results = "{}.py.txt".format(test[:-len(suffix)])
            wished_results = open(wished_results).read().strip()
            actual_results = run_py_file(test)
            if wished_results == actual_results:
                # print("PASSED {}".format(test))
                pass
            else:
                print("FAILED {}".format(test))
                print("Program:")
                print(program)
                print("Wished: {}".format(repr(wished_results)))
                print("Actual: {}".format(repr(actual_results)))
                print("----------")
        except FileNotFoundError as e:
            print("FAILED {}".format(test))
            print("Program:")
            print(program)
            print("No expected output.")
            print("----------")

    suffix = ".scala"
    # i = 0
    for test in glob.glob("./test/{}/*{}".format(test_path, suffix)):
        # i = i + 1
        # if i > 10:
        #     break
        program = open(test).read()
        try:
            wished_results = "{}.scala.txt".format(test[:-len(suffix)])
            wished_results = open(wished_results).read().strip()
            actual_results = run_scala_file(test)
            if wished_results == actual_results:
                # print("PASSED {}".format(test))
                pass
            else:
                print("FAILED {}".format(test))
                print("Program:")
                print(program)
                print("Wished: {}".format(repr(wished_results)))
                print("Actual: {}".format(repr(actual_results)))
                print("----------")
        except FileNotFoundError as e:
            print("FAILED {}".format(test))
            print("Program:")
            print(program)
            print("No expected output.")
            print(e)
            print("----------")