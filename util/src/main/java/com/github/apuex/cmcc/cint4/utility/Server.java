package com.github.apuex.cmcc.cint4.utility;

import java.util.Map;

import com.github.apuex.cmcc.cint4.EnumType;
import com.github.apuex.cmcc.cint4.TIDToSignalTypeMap;

import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.ChannelFuture;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelOption;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.group.ChannelGroup;
import io.netty.channel.group.DefaultChannelGroup;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.util.concurrent.GlobalEventExecutor;

public class Server {
	static final ChannelGroup channels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE);
	public static void launch(final Map<String, String> params) {
		EventLoopGroup bossGroup = new NioEventLoopGroup();
		EventLoopGroup workerGroup = new NioEventLoopGroup();
		try {
			ServerBootstrap bootstrap = new ServerBootstrap();
			final TIDToSignalTypeMap tidMap = (v -> EnumType.AI);
			bootstrap.group(bossGroup, workerGroup).channel(NioServerSocketChannel.class)
					.childHandler(new ChannelInitializer<SocketChannel>() {
						@Override
						public void initChannel(SocketChannel ch) throws Exception {
							ch.pipeline()
								.addLast(new ByteToCInt4MessageDecoder(tidMap))
								.addLast(new CInt4MessageToByteEncoder(tidMap))
								.addLast(new ServerHandler(params, channels));
						}
					})
					.option(ChannelOption.SO_BACKLOG, 1024)
					.option(ChannelOption.SO_REUSEADDR, true)
					.childOption(ChannelOption.SO_KEEPALIVE, true);

			// Bind and start to accept incoming connections.
			ChannelFuture f = bootstrap.bind(params.get("server-host"), Integer.parseInt(params.get("server-port")))
					.sync();

			// Wait until the server socket is closed,
			// and shut down the server.
			f.channel().closeFuture().sync();

		} catch (Exception e) {
			e.printStackTrace();
			workerGroup.shutdownGracefully();
			bossGroup.shutdownGracefully();
		}
	}
}
