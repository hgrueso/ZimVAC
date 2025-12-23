library(haven)
library(dplyr)

cat("========================================\n")
cat("Comparing ZIMVACCombined.dta datasets\n")
cat("========================================\n\n")

# Load both datasets
cat("Loading datasets...\n")
new_data <- read_dta("ZIMVACCombined.dta")
old_data <- read_dta("ZIMVACCombined.dta-2024")

cat("✓ Both datasets loaded successfully\n\n")

# Compare dimensions
cat("----------------------------------------\n")
cat("DIMENSION COMPARISON\n")
cat("----------------------------------------\n")
cat(sprintf("Old dataset: %d rows × %d columns\n", nrow(old_data), ncol(old_data)))
cat(sprintf("New dataset: %d rows × %d columns\n", nrow(new_data), ncol(new_data)))

if (nrow(new_data) != nrow(old_data)) {
  cat(sprintf("⚠ WARNING: Row count differs by %d rows\n", nrow(new_data) - nrow(old_data)))
} else {
  cat("✓ Row counts match\n")
}

if (ncol(new_data) != ncol(old_data)) {
  cat(sprintf("⚠ WARNING: Column count differs by %d columns\n", ncol(new_data) - ncol(old_data)))
} else {
  cat("✓ Column counts match\n")
}
cat("\n")

# Compare column names
cat("----------------------------------------\n")
cat("COLUMN NAME COMPARISON\n")
cat("----------------------------------------\n")
old_cols <- colnames(old_data)
new_cols <- colnames(new_data)

cols_only_in_old <- setdiff(old_cols, new_cols)
cols_only_in_new <- setdiff(new_cols, old_cols)

if (length(cols_only_in_old) > 0) {
  cat("⚠ Columns in OLD dataset but NOT in new:\n")
  for (col in cols_only_in_old) {
    cat(sprintf("  - %s\n", col))
  }
} else {
  cat("✓ No columns removed\n")
}

if (length(cols_only_in_new) > 0) {
  cat("⚠ Columns in NEW dataset but NOT in old:\n")
  for (col in cols_only_in_new) {
    cat(sprintf("  + %s\n", col))
  }
} else {
  cat("✓ No columns added\n")
}

if (length(cols_only_in_old) == 0 && length(cols_only_in_new) == 0) {
  cat("✓ Column names are identical\n")
}
cat("\n")

# Compare common columns
common_cols <- intersect(old_cols, new_cols)

if (length(common_cols) > 0) {
  cat("----------------------------------------\n")
  cat("DATA VALUE COMPARISON (Common Columns)\n")
  cat("----------------------------------------\n")
  cat(sprintf("Comparing %d common columns...\n\n", length(common_cols)))

  differences_found <- FALSE

  for (col in common_cols) {
    # Check if columns are identical
    old_vec <- old_data[[col]]
    new_vec <- new_data[[col]]

    # Handle the case where row counts differ
    min_rows <- min(length(old_vec), length(new_vec))

    if (length(old_vec) != length(new_vec)) {
      cat(sprintf("⚠ Column '%s': Different lengths (old: %d, new: %d)\n",
                  col, length(old_vec), length(new_vec)))
      differences_found <- TRUE
      next
    }

    # Compare values (accounting for NAs)
    are_equal <- all.equal(old_vec, new_vec)

    if (!isTRUE(are_equal)) {
      differences_found <- TRUE

      # Count differences
      if (is.numeric(old_vec) && is.numeric(new_vec)) {
        na_old <- sum(is.na(old_vec))
        na_new <- sum(is.na(new_vec))

        # Check for NA differences
        if (na_old != na_new) {
          cat(sprintf("⚠ Column '%s': Different NA counts (old: %d, new: %d)\n",
                      col, na_old, na_new))
        }

        # Check for value differences (excluding NAs)
        non_na_mask <- !is.na(old_vec) & !is.na(new_vec)
        if (sum(non_na_mask) > 0) {
          diff_mask <- non_na_mask & (old_vec != new_vec)
          num_diffs <- sum(diff_mask, na.rm = TRUE)

          if (num_diffs > 0) {
            cat(sprintf("⚠ Column '%s': %d rows with different values (%.2f%%)\n",
                        col, num_diffs, 100 * num_diffs / nrow(new_data)))

            # Show summary statistics for numeric columns
            old_mean <- mean(old_vec, na.rm = TRUE)
            new_mean <- mean(new_vec, na.rm = TRUE)
            cat(sprintf("    Old mean: %.4f, New mean: %.4f\n", old_mean, new_mean))
          }
        }
      } else {
        # For non-numeric columns
        na_old <- sum(is.na(old_vec))
        na_new <- sum(is.na(new_vec))

        if (na_old != na_new) {
          cat(sprintf("⚠ Column '%s': Different NA counts (old: %d, new: %d)\n",
                      col, na_old, na_new))
        }

        # Count non-NA differences
        non_na_mask <- !is.na(old_vec) & !is.na(new_vec)
        if (sum(non_na_mask) > 0) {
          if (is.character(old_vec) || is.character(new_vec)) {
            diff_count <- sum(old_vec[non_na_mask] != new_vec[non_na_mask], na.rm = TRUE)
          } else {
            diff_count <- sum(!identical(old_vec[non_na_mask], new_vec[non_na_mask]))
          }

          if (diff_count > 0) {
            cat(sprintf("⚠ Column '%s': %d rows with different values\n", col, diff_count))
          }
        }
      }
    }
  }

  if (!differences_found) {
    cat("✓ All common columns have identical values!\n")
  }
  cat("\n")
}

# Summary
cat("========================================\n")
cat("SUMMARY\n")
cat("========================================\n")

total_issues <- length(cols_only_in_old) + length(cols_only_in_new)
if (nrow(new_data) != nrow(old_data)) total_issues <- total_issues + 1
if (ncol(new_data) != ncol(old_data)) total_issues <- total_issues + 1

if (total_issues == 0 && !differences_found) {
  cat("✓ DATASETS ARE IDENTICAL\n")
} else {
  cat(sprintf("⚠ DATASETS DIFFER\n"))
  if (length(cols_only_in_old) > 0) {
    cat(sprintf("  - %d columns removed\n", length(cols_only_in_old)))
  }
  if (length(cols_only_in_new) > 0) {
    cat(sprintf("  - %d columns added\n", length(cols_only_in_new)))
  }
  if (exists("differences_found") && differences_found) {
    cat("  - Some column values differ\n")
  }
}
cat("========================================\n")
