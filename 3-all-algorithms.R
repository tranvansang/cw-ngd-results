require(plyr)
require(dplyr)

all_df <- data.frame()

for (ds in c('mnist', 'fmnist', 'cifar10', 'stl10')) {
	for (optimizer in c('cwngd', 'kfac', 'adam', 'msgd')) {
		for (seed_no in 0:4) {
			for (mode in c('test', 'step', 'train')) {
				file_path <- paste0('logs/stat-exp3-', ds, '-', optimizer, '-', seed_no, '-', mode, '.csv')
				lines <- readLines(file_path, warn = FALSE)
				lines <- lines[!grepl('None', lines)]
				if (length(lines) <= 1) {
					cat(paste0('Skip ', file_path, '\n'))
					next
				}
				df <- read.table(text=lines, header = TRUE, sep = '\t')
				df$metric <- mode
				df$optimizer <- optimizer
				df$seed_no <- seed_no
				df$dataset <- ds
				all_df <- rbind.fill(all_df, df)

				if (mode == 'train' && nrow(df) < 100) {
					cat(paste0('Not enough row ', file_path, '\n'))
				}
			}
		}
	}
}


filter_df <- function (optimizer_name, metric_name, seed_no_name, dataset_name) {
	all_df %>% filter(
		metric == metric_name,
		seed_no == seed_no_name,
		optimizer == optimizer_name,
		dataset == dataset_name,
	)
}

quantity_table <- function() {
	quantity_df <<- data.frame(
		end_acc = NA,
		steps_until_90 = NA,
		variance_after_90 = NA,
		algorithm = NA,
		mode = NA,
		dataset = NA,
		seed_no = NA,
		nsteps = NA
	)
	for (seed_no in 0:4) {
		for (ds in c('mnist', 'fmnist', 'cifar10', 'stl10')) {
			for (algo in c('cwngd', 'adam', 'msgd', 'kfac')) {
				test_df <<- filter_df(optimizer_name = algo, metric_name = 'test', seed_no_name = seed_no, dataset_name = ds)
				train_df <<- filter_df(optimizer_name = algo, metric_name = 'step', seed_no_name = seed_no, dataset_name = ds)
				if (nrow(test_df) > 0 && nrow(train_df) > 0) {
					nsteps <- nrow(train_df)
					train_acc <- train_df$acc[nsteps]
					train_steps <- which(train_df$acc >= train_acc * .9)[1]
					which_train_small <- which(train_df$acc < train_acc * .9)
					var_train_steps <- which_train_small[length(which_train_small)] - train_steps + 1

					quantity_df <<- quantity_df %>%
						add_row(
							dataset = ds,
							algorithm = algo,
							seed_no = seed_no,
							end_acc = train_acc,
							steps_until_90 = train_steps,
							variance_after_90 = var_train_steps,
							mode = 'train',
							nsteps = nsteps
						)

					nsteps <- nrow(test_df)
					test_acc <- test_df$acc[nsteps]
					test_steps <- which(test_df$acc >= test_acc * .9)[1]
					which_test_small <- which(test_df$acc < test_acc * .9)
					var_test_steps <- which_test_small[length(which_test_small)] - test_steps + 1

					quantity_df <<- quantity_df %>%
						add_row(
							dataset = ds,
							algorithm = algo,
							seed_no = seed_no,
							end_acc = test_acc,
							steps_until_90 = test_steps,
							variance_after_90 = var_test_steps,
							mode = 'test',
							nsteps = nsteps
						)
				}
			}
		}
	}
	quantity_df <<- quantity_df %>% slice(-1)
}
quantity_table()

cat(paste('Dataset', 'Label', 'CWNGD', 'variance', 'Adam', 'variance', 'SGD', 'variance', 'KFAC', 'variance', sep='\t'), '\n')
cat('\n')
cat('Average', '\n')
for (ds in c('mnist', 'fmnist', 'cifar10', 'stl10')) {
	acc_train_str <- NULL
	steps_train_str <- NULL
	acc_test_str <- NULL
	steps_test_str <- NULL

	for (algo in c('cwngd', 'adam', 'msgd', 'kfac')) {
		sub_df <- quantity_df %>%
			filter(
				dataset == ds,
				algorithm == algo
			)
		if (nrow(sub_df) > 0) {
			train_df <- sub_df %>% filter(mode == 'train')
			nsteps <- train_df$nsteps[1]
			train_acc <- mean(train_df$end_acc)
			train_steps <- mean(train_df$steps_until_90)
			var_train_steps <- mean(train_df$variance_after_90)

			test_df <- sub_df %>% filter(mode == 'test')
			test_acc <- mean(test_df$end_acc)
			test_steps <- mean(test_df$steps_until_90)
			var_test_steps <- mean(test_df$variance_after_90)

			acc_train_str <- c(acc_train_str, sprintf('%.4f %%', train_acc), '')
			steps_train_str <- c(steps_train_str, paste(train_steps, '/', nsteps, sep=' '), paste0('\'+', var_train_steps))
			acc_test_str <- c(acc_test_str, sprintf('%.4f %%', test_acc), '')
			steps_test_str <- c(steps_test_str, paste(test_steps, '/', nsteps, sep=' '), paste0('\'+', var_test_steps))
		} else {
			acc_train_str <- c(acc_train_str, '-', '')
			steps_train_str <- c(steps_train_str, '-', '-')
			acc_test_str <- c(acc_test_str, '-', '')
			steps_test_str <- c(steps_test_str, '-', '-')
		}
	}
	acc_train_str <- paste(acc_train_str, collapse = '\t')
	steps_train_str <- paste(steps_train_str, collapse = '\t')
	acc_test_str <- paste(acc_test_str, collapse = '\t')
	steps_test_str <- paste(steps_test_str, collapse = '\t')

	cat(paste(
		paste(ds, 'End train acc', acc_train_str, sep='\t', collapse = '\t'),
		paste(ds, 'Steps until 90%', steps_train_str, sep='\t', collapse = '\t'),
		paste(ds, 'End test acc', acc_test_str, sep='\t', collapse = '\t'),
		paste(ds, 'Steps until 90%', steps_test_str, sep='\t', collapse = '\t'),
		sep='\n'
	))
	cat('\n')
	cat('\n')
}
