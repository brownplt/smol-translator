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
    actual_result = " ".join(s.strip() for s in actual_result)
    actual_result = actual_result.strip().split("None")
    actual_result = [s for s in actual_result if s != '']
    actual_result = " ".join(s.strip() for s in actual_result)
    return actual_result


def run_scala_file(test):
    command = [
        "scala",
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


suffix = ".js"
# i = 0
for test in glob.glob("./test/test_cases/*{}".format(suffix)):
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
for test in glob.glob("./test/test_cases/*{}".format(suffix)):
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
for test in glob.glob("./test/test_cases/*{}".format(suffix)):
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
        print("----------")