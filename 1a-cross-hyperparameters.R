# in the real experiment, we ran 4 jobs separatedly with different range of learning rates and damping rates
# this script combines the results and plot them, making the plot in the paper
# to run this script:
# - (cd logs && tar xvf exp1a.tar.xz)
# - Rscript 1a-cross-hyperparameters.R
# - Plot is saved at 1a-cross-hyperparameters.png

require(ggplot2)
require(viridis)
require(svglite)
require(scales)
require(plyr)
require(dplyr)

# #=============
# # read props
# #=============
props_df1 <- read.csv('logs/exp3a-run1.csv', sep='\t') %>% mutate(run_no = 1)
props_df2 <- read.csv('logs/exp3a-run2.csv', sep='\t') %>% mutate(run_no = 2)
props_df3 <- read.csv('logs/exp3a-run3.csv', sep='\t') %>% mutate(run_no = 3)
props_df4 <- read.csv('logs/exp3a-run4.csv', sep='\t') %>% mutate(run_no = 4)

props_df <- rbind.fill(props_df1, props_df2, props_df3, props_df4) %>%
	select(
		run_no
		, pid
		, LEARNING_RATE
		, CW_NGD_DAMPING
		, reason
		, epoch_no
		, step_no
		, train_acc
		, test_acc
	) %>%
	rename(c('lr' = 'LEARNING_RATE', 'damping' = 'CW_NGD_DAMPING'))

# #=============
# # read individual running results
# #=============

df_list <- list()
for (row_no in seq_len(nrow(props_df))) {
	if (props_df$reason[row_no] != 'exception') {
		for (mode in c('test', 'train', 'step')) {
			pid <- props_df$pid[row_no]
			run_no <- props_df$run_no[row_no]
			file_path <- paste0('logs/run', run_no, '-', pid, '-', mode, '.csv')
			if (file.exists(file_path)) {
				df <- read.table(text=readLines(file_path, warn = FALSE), header = TRUE, sep = '\t')
				df$mode <- mode
				df$pid <- pid
				df$run_no <- run_no
				df <- df %>% filter(loss != 'None') %>% mutate(loss = as.numeric(loss), acc = as.numeric(acc))
				df_list[[paste(pid, run_no, mode, sep = '-')]] <- df
				# df_list <- c(df_list, list(df))
			} else cat(paste('Missing', file_path, '\n'))
		}
	}
}
# all_df <- bind_rows(df_list)

# #=============
# # get aggregated data
# #=============
best_row <- props_df %>% filter(reason == 'max_acc') %>% arrange(-step_no) %>% tail(1)

best_step_no <- best_row$step_no - 1
best_pid <- best_row$pid
best_run_no <- best_row$run_no
best_epoch_no <- best_row$epoch_no

props_df$end_train_acc <- NA
props_df$end_test_acc <- NA
props_df$end_step_acc <- NA

props_df$max_train_acc <- NA
props_df$max_test_acc <- NA
props_df$max_step_acc <- NA

for (row_no in seq_len(nrow(props_df))) {
	if (!grepl('exception', props_df$reason[row_no])) {
		for (mode in c('test', 'train', 'step')) {
			pid_no <- props_df$pid[row_no]
			run_no_val <- props_df$run_no[row_no]

			props_df$end_train_acc[row_no] <- df_list[[paste(pid_no, run_no_val, 'train', sep = '-')]]$acc[best_epoch_no]
			props_df$end_test_acc[row_no] <- df_list[[paste(pid_no, run_no_val, 'test', sep = '-')]]$acc[best_step_no]
			props_df$end_step_acc[row_no] <- df_list[[paste(pid_no, run_no_val, 'step', sep = '-')]]$acc[best_step_no]

			props_df$max_train_acc[row_no] <- max(df_list[[paste(pid_no, run_no_val, 'train', sep = '-')]]$acc[1:best_epoch_no])
			props_df$max_test_acc[row_no] <- max(df_list[[paste(pid_no, run_no_val, 'test', sep = '-')]]$acc[1:best_step_no])
			props_df$max_step_acc[row_no] <- max(df_list[[paste(pid_no, run_no_val, 'step', sep = '-')]]$acc[1:best_step_no])

			# props_df$end_train_acc[row_no] <- all_df %>% filter(pid == pid_no & run_no == run_no_val & mode == 'train') %>% slice(best_epoch_no) %>% pull(acc)
			# props_df$end_test_acc[row_no] <- all_df %>% filter(pid == pid_no & run_no == run_no_val & mode == 'test') %>% slice(best_step_no) %>% pull(acc)
			# props_df$end_step_acc[row_no] <- all_df %>% filter(pid == pid_no & run_no == run_no_val & mode == 'step') %>% slice(best_step_no) %>% pull(acc)
			#
			# props_df$max_train_acc[row_no] <- all_df %>% filter(pid == pid_no & run_no == run_no_val & mode == 'train') %>% slice(1:best_epoch_no) %>% pull(acc) %>% max()
			# props_df$max_test_acc[row_no] <- all_df %>% filter(pid == pid_no & run_no == run_no_val & mode == 'test') %>% slice(1:best_step_no) %>% pull(acc) %>% max()
			# props_df$max_step_acc[row_no] <- all_df %>% filter(pid == pid_no & run_no == run_no_val & mode == 'step') %>% slice(1:best_step_no) %>% pull(acc) %>% max()
		}
	}
}

# #=============
# # plot
# #=============
is_train <- FALSE
metric <- if (is_train) 'max_train_acc' else 'max_test_acc'
metric_display_name <- if (is_train) 'Max training accuracy (%)' else 'Max test accuracy (%)'

propx <- 'lr'
propy <- 'damping'

sub_df <- data.frame(
	x = props_df[[propx]],
	y = props_df[[propy]],
	val = props_df[[metric]],
	reason = props_df$reason,
	step_no = props_df$step_no
)
no_exception_df <- sub_df %>% filter(reason != 'exception')
exception_df <- sub_df %>% filter(reason == 'exception')
best_df <- sub_df %>% filter(reason == 'max_acc') %>% arrange(-step_no) %>% tail(1)
best_df$idx <- seq_len(nrow(best_df))

plt <- ggplot(no_exception_df) +
	geom_point(aes(
		x = x
		,y = y
		,color = val
	)) + labs(
	x = propx,
	y = propy,
	color=if (is_train) 'Training Accuracy' else 'Test Accuracy',
	title = paste(metric_display_name, '- Learning Rate versus Damping Rate'),
) +
	scale_color_viridis(discrete = FALSE) +
	annotate("point", x = best_df$x, y = best_df$y
		, colour = "red"
		, size = best_df$idx
	) +
	annotate(
		"point",
		x = exception_df$x,
		y = exception_df$y
		, color = 'gray'
	)
plt <- plt + scale_x_log10(breaks = c(1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, .1, 1, 10), labels = label_log(digits = 2))
plt <- plt + labs(x = 'Learning Rate (log10)')
plt <- plt + scale_y_log10(breaks = c(1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, .1, 1, 10), , labels = label_log(digits = 2))
plt <- plt + labs(y = 'Damping Rate (log10)')
plt <- plt + theme(legend.title = element_text(margin = margin(t = 0, r = 0, b = 0, l = -10), angle = 90))
plt

filepath <- '1a-cross-hyperparameters.png'
ggsave(
	filepath,
	plt,
	width=7, height=5.7, unit='in'
)
cat(paste('Saved', filepath, '\n'))
