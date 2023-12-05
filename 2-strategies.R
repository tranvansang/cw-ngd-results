require(plyr)
require(dplyr)
require(ggplot2)

all_algos <- c(
	'Semantics-aware'

	,'Random size 9 seed 1'
	,'Random size 9 seed 2'
	,'Random size 9 seed 3'
	,'Random size 9 seed 4'
	,'Random size 9 seed 5'

	,'Random size 1'
	,'Random size 2'
	,'Random size 3'
	,'Random size 4'
	,'Random size 5'
	,'Random size 6'
	,'Random size 7'
	,'Random size 8'
	,'Random size 16'
	,'Random size 32'

	,'Sequential size 8'
	,'Sequential size 9'
	,'Sequential size 16'
	,'Sequential size 32'
	,'Sequential size 64'
)
all_df <- data.frame()

read_strategy_df <- function(strategy) {
	for (mode in c('test', 'step', 'train')) {
		file_path <- paste0('logs/stat-exp2-strategy_', strategy, '_', mode, '.csv')
		lines <- readLines(file_path, warn = FALSE)
		df <- read.table(text=lines, header = TRUE, sep = '\t')
		df$metric <- mode
		df$strategy <- strategy
		df$algo <- factor(c(
			'0' = 'Semantics-aware'
			,'-1' = 'Random size 1'
			,'-2' = 'Random size 2'
			,'-3' = 'Random size 3'
			,'-4' = 'Random size 4'
			,'-5' = 'Random size 5'
			,'-6' = 'Random size 6'
			,'-7' = 'Random size 7'
			,'-8' = 'Random size 8'
			,'-16' = 'Random size 16'
			,'-32' = 'Random size 32'
			,'8' = 'Sequential size 8'
			,'9' = 'Sequential size 9'
			,'16' = 'Sequential size 16'
			,'32' = 'Sequential size 32'
			,'64' = 'Sequential size 64'
		)[toString(strategy)], levels = all_algos)

		all_df <<- rbind.fill(all_df, df)
	}
}
read_strategy_df(0)
read_strategy_df(-1)
read_strategy_df(-2)
read_strategy_df(-3)
read_strategy_df(-4)
read_strategy_df(-5)
read_strategy_df(-6)
read_strategy_df(-7)
read_strategy_df(-8)
read_strategy_df(-16)
read_strategy_df(-32)
read_strategy_df(8)
read_strategy_df(9)
read_strategy_df(16)
read_strategy_df(32)

# to_smooth <- TRUE
to_smooth <- FALSE
liner <- if (to_smooth) geom_smooth else geom_line

metric_name <- 'test'
# metric_name <- 'train'

plt <- ggplot(
	all_df %>% filter(metric == metric_name),
	aes(x = step, y = acc, color = algo)
) +
	labs(x='Step', y='CIFAR-10 Testing Accuracy', title='Compare different decomposition strategies', color='Decomposition Strategy') +
	theme(
		strip.text.x = element_blank(),
		strip.background = element_rect(colour="white", fill="white"),
		legend.position=c(.5,.4)
	) +

	scale_color_manual(values=c(
		'Red'

		,c(
			'#E9CE63'
			,'#DECC61'
			,'#D4C95F'
			,'#C9C75E'
			,'#BEC45C'
			,'#B3C25B'
			,'#A9BF5A'
			,'#9FBC5A'
			,'#95B958'
			,'#8BB657'
			,'#81B357'
			,'#78B056'
			,'#6EAC56'
			,'#65A856'
			,'#5CA455'
			,'#55A154'
			,'#4E9D53'
			,'#499851'
			,'#449450'
			,'#3F904F'
			,'#3B8C4D'
			,'#38884C'
			,'#34844A'
			,'#2F7F46'
			,'#2A7B43'
			,'#257740'
			,'#1F733D'
			,'#196F39'
			,'#146C36'
		)[c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)]
		, '#0ea5e9' # sky-500
		, '#14b8a6' # tear-500
		,'#c084fc'
		, '#6b21a8'
		, 'black'
	)) +
	guides(linetype = guide_legend(override.aes = list(size = 5)))

for (to_zoom in c(FALSE, TRUE)) {
	if (to_zoom) {
		plt1 <- plt + liner()
		plt1 <- plt1 + xlim(c(4000, 4500)) + ylim(c(77.5, 87.5))
	} else {
		plt1 <- plt + liner(linewidth=.2)
	}

	filepath <- paste0(
		'2-strategies',
		if (to_zoom) '-zoom' else '',
		'.png'
	)
	ggsave(
		filepath,
		plt1,
		width=7, height=5.7, unit='in'
	)
	cat(paste('Saved', filepath, '\n'))
}

plt1
