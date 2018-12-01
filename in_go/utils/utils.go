package utils

import ()

// Check ...
// panic if error
func Check(err error) {
	if err != nil {
		panic(err)
	}
}
