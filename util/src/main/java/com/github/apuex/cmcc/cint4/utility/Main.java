package com.github.apuex.cmcc.cint4.utility;

import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;

import static java.lang.System.out;

public class Main {
	public static void main(String args[]) throws Exception {
		final Options options = options();

		CommandLineParser parser = new DefaultParser();
		CommandLine cmd = parser.parse(options, args);

		if (cmd.hasOption("h")) {
			HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp("re-send-alarm <options>", options);
			printOptions(defaultOptions());
		} else {
			final Map<String, String> params = defaultOptions();

			options.getOptions().stream().forEach(o -> {
				if (cmd.hasOption(o.getLongOpt())) {
					params.put(o.getLongOpt(), cmd.getOptionValue(o.getOpt()));
				}
			});

			printOptions(params);
			for(String arg : cmd.getArgList()) {
			  out.printf("  %s\n", arg);
			  // TODO: perform task.
			  
			}
		}
	}

	public static Map<String, String> defaultOptions() {
		return new TreeMap<String, String>() {
			private static final long serialVersionUID = 1L;
			{
				put("server-host", "localhost");
				put("server-port", "8110");
				put("server-user", "user");
				put("server-pass", "1234");
				put("mq-host", "localhost");
				put("mq-port", "7676");
				put("mq-user", "guest");
				put("mq-pass", "guest");
			}
		};
	}

	public static void printOptions(Map<String, String> options) {
		out.println("current options are:");
		final int maxLength = options.entrySet().stream().map(x -> x.getKey().length()).max(Integer::compare).get() + 1;

		options.entrySet().forEach(e -> out.printf("  --%s = %s\n", paddingRight(e.getKey(), maxLength), e.getValue()));
	}

	public static String paddingRight(String s, int maxWidth) {
		int length = s.length();
		StringBuilder sb = new StringBuilder();
		sb.append(s);
		if (length < maxWidth) {
			for (int i = length; i < maxWidth; ++i) {
				sb.append(' ');
			}
		}
		return sb.toString();
	}

	public static Options options() {
		final Options options = new Options();
		options.addOption(new Option("s", "server-host", true, "C Interface server server host/ip"));
		options.addOption(new Option("p", "server-port", true, "C Interface server server port"));
		options.addOption(new Option(null, "server-user", true, "C Interface server user name"));
		options.addOption(new Option(null, "server-pass", true, "C Interface server password"));
		options.addOption(new Option(null, "mq-host", true, "message broker host"));
		options.addOption(new Option(null, "mq-port", true, "message broker port"));
		options.addOption(new Option(null, "mq-user", true, "message broker user name"));
		options.addOption(new Option(null, "mq-pass", true, "message broker password"));
		options.addOption(new Option("h", "help", false, "print help message"));
		return options;
	}
}