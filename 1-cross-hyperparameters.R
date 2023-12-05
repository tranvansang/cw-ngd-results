require(ggplot2)
require(viridis)
require(svglite)
require(scales)
require(plyr)
require(dplyr)

#=============
# read props
#=============
lines <- readLines('logs/1-cross-hyperparameters.csv', warn = FALSE) %>% tail(-6) %>% head(-3)
props_df <- read.table(text=lines, header = FALSE, sep = '\t', fill = TRUE)
names(props_df) <- c(
	'pid_name', 'pid'
	,'gpu_id_name', 'gpu_id'
	,'lr_name', 'lr'
	,'damping_name', 'damping'
	,'reason_name', 'reason'
	,'epoch_no_name', 'epoch_no' # message for exeption reason
	,'step_no_name', 'step_no'
	,'train_loss_name', 'train_loss'
	,'train_acc_name', 'train_acc'
	,'test_loss_name', 'test_loss'
	,'test_acc_name', 'test_acc'
)
#=============
# refine exception cases
#=============
props_df <- props_df %>%
	mutate(message = ifelse(reason == 'exception', epoch_no, NA)) %>%
	mutate(epoch_no = ifelse(reason == 'exception', NA, as.numeric(epoch_no)))

#=============
# read individual running results
#=============
df_list <- list()
# all_df <- data.frame()

for (row_no in seq_len(nrow(props_df))) {
	if (props_df$reason[row_no] != 'exception') {
		for (mode in c('test', 'train', 'step')) {
			pid <- props_df$pid[row_no]
			file_path <- paste0('logs/stat-exp1-', pid, '-', mode, '.csv')
			if (file.exists(file_path)) {
				df <- read.table(text=readLines(file_path, warn = FALSE), header = TRUE, sep = '\t')
				df$mode <- mode
				df$pid <- pid
				df_list[[paste(pid, mode, sep = '-')]] <- df
				# all_df <- rbind.fill(all_df, df)
			} else cat(paste('Missing', file_path, '\n'))
		}
	}
}

#=============
# get aggregated data
#=============
best_pid <- tail(props_df[props_df$reason == 'max_acc', 'pid'], 1)
best_step_no <- tail(props_df[props_df$reason == 'max_acc', 'step_no'], 1) - 1
best_epoch_no <- tail(props_df[props_df$reason == 'max_acc', 'epoch_no'], 1)

props_df$end_train_acc <- NA
props_df$end_test_acc <- NA
props_df$end_step_acc <- NA

props_df$max_train_acc <- NA
props_df$max_test_acc <- NA
props_df$max_step_acc <- NA

for (row_no in seq_len(nrow(props_df))) {
	if (props_df$reason[row_no] != 'exception') {
		for (mode in c('test', 'train', 'step')) {
			pid_no <- props_df$pid[row_no]
			props_df$end_train_acc[row_no] <- df_list[[paste(pid_no, 'train', sep = '-')]]$acc[best_epoch_no]
			props_df$end_test_acc[row_no] <- df_list[[paste(pid_no, 'test', sep = '-')]]$acc[best_step_no]
			props_df$end_step_acc[row_no] <- df_list[[paste(pid_no, 'step', sep = '-')]]$acc[best_step_no]

			props_df$max_train_acc[row_no] <- max(df_list[[paste(pid_no, 'train', sep = '-')]]$acc[1:best_epoch_no])
			props_df$max_test_acc[row_no] <- max(df_list[[paste(pid_no, 'test', sep = '-')]]$acc[1:best_step_no])
			props_df$max_step_acc[row_no] <- max(df_list[[paste(pid_no, 'step', sep = '-')]]$acc[1:best_step_no])

			# props_df$end_train_acc[row_no] <- all_df %>% filter(pid == pid_no, mode == 'train') %>% slice(best_epoch_no) %>% pull(acc)
			# props_df$end_test_acc[row_no] <- all_df %>% filter(pid == pid_no, mode == 'test') %>% slice(best_step_no) %>% pull(acc)
			# props_df$end_step_acc[row_no] <- all_df %>% filter(pid == pid_no, mode == 'step') %>% slice(best_step_no) %>% pull(acc)
			#
			# props_df$max_train_acc[row_no] <- all_df %>% filter(pid == pid_no, mode == 'train') %>% slice(1:best_epoch_no) %>% pull(acc) %>% max()
			# props_df$max_test_acc[row_no] <- all_df %>% filter(pid == pid_no, mode == 'test') %>% slice(1:best_step_no) %>% pull(acc) %>% max()
			# props_df$max_step_acc[row_no] <- all_df %>% filter(pid == pid_no, mode == 'step') %>% slice(1:best_step_no) %>% pull(acc) %>% max()
		}
	}
}

#=============
# plot
#=============
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

filepath <- '1-cross-hyperparameters.png'
ggsave(
	filepath,
	plt,
	width=7, height=5.7, unit='in'
)
cat(paste('Saved', filepath, '\n'))
