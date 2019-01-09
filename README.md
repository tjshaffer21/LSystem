A simple implementation of the Lindenmayer system.

# Dependencies

* alexandria
* iterate
* cl-yaml
* cl-glfw3

* lisp-unit - ```lsystem-test.asd``` only

# Loading

If you want to load it into a running interpreter then it can be done by loading the ```lsystem.asd``` file with ASDF, and then calling quickload to load the system. ```lsystem-test.asd``` can be loaded, similarly, for unit testing.

# Execution

In interpreter:

```(lsystem:main "data/tree1.yaml" "data/turtle_t1.yaml")```

In terminal (using CCL):

```ccl -l main.lisp -- -rules=rules.yaml -translate=translate.yaml```,
where ```ccl``` is path to the CCL executable.

# Data Files

You can find examples in the data\ directory. Do note that values are senstive, and incorrect values will lead to undefined behavior.

# Limitations

* Extremely large L-Systems will fail due to string size limitations.
* Currently, large iterations can be slow in rendering.