strings <- c("a", "ab", "acb", "accb", "acccb", "accccb", "acccc.b")
strings
grep("ac*b", strings, value = TRUE)
grep("ac+b", strings, value = TRUE)
grep("ac+\\.b", strings, value=TRUE)
grep("ac?b", strings, value = TRUE)
grep("ac{2}b", strings, value = TRUE)
grep("ac{2,}b", strings, value = TRUE)
grep("ac{2,3}b", strings, value = TRUE)
stringr::str_extract_all(strings, "ac{2,3}b", simplify = TRUE)