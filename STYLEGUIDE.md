# Haskell

 - No line limit.

### Language extensions

 - Alphabetized and one per line.

```
{-# LANGUAGE OverloadedStrings #-}
```

```
{-# LANGUAGE GADTs,
             OverloadedStrings #-}
```

### Imports

### Module definitions

# JavaScript

 - 80-ish character lines.
 - Only use `const`/`let`, never `var`.
 - Use `import`/`exports`, not `module.exports` or `require`.
 - No mutated global variables.

### Import organization

 - Import external modules, then utility modules, then directly related modules.

```
import React from "react";

import * as utils from "../utils.js";

import TestComponent from "./TestComponent.js";
```

### Arrays / Objects

 - Trailing commas if elements are on different lines.
 - Non-empty objects should be on separate lines.
 - Quote object keys when being used as data, don't quote when being used as
   accessors.

```
const a = [1, 2, 3];
```

```
const a = [
    1,
    2,
    3,
];
```

```
const b = {};
```

```
const b = {
    a: 1,
    b: 3,
};
```

```
const b = {
    "data-something": "blah",
};
```

### Function calls / argument alignment

 - Put final parentheses on a new line, lined up with the function name.
 - 

```
func(blah, blah, blah);
```

```
func(
    blah, blah, blah, blah
);
```

```
func(
    blah, blah, func(blah), blah,
    blah, blah
);
```

```
func(
    blah, blah, func(
        blah, blah
    ), blah,
    blah, blah
);
```

```
func(
    blah, blah, (arg, arg) => {
        blah;
    }, blah,
    blah, blah
);
```

### React

 - Always use classes, not React.createClass
 - Try to keep state in flux stores

```
return <blah args={blah} />;
```

```
return <blah arg1={blah}
             arg2={blah} />;
```

```
return (
    <blah args={blah}>
        <stuff>
    </blah>
);
```

```
return (
    <blah arg1={hello}
          args={blah} >
        <stuff>
    </blah>
);
```

```
return <blah arg1={func(blah, function() {
                 body;
             })}
```
