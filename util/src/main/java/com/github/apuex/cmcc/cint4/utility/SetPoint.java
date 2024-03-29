package com.github.apuex.cmcc.cint4.utility;

import com.github.apuex.cmcc.cint4.EnumType;
import com.github.apuex.cmcc.cint4.TIDToSignalTypeMap;
import com.github.apuex.cmcc.cint4.codec.ByteToCInt4MessageDecoder;
import com.github.apuex.cmcc.cint4.codec.CInt4MessageToByteEncoder;
import io.netty.bootstrap.Bootstrap;
import io.netty.channel.ChannelFuture;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioSocketChannel;

import java.util.Map;

public class SetPoint {
	public static void launch(final Map<String, String> params) {
		EventLoopGroup workerGroup = new NioEventLoopGroup();
		Bootstrap bootstrap = new Bootstrap();

		try {
			final TIDToSignalTypeMap tidMap = (v -> EnumType.AI);
			bootstrap.group(workerGroup).channel(NioSocketChannel.class)
					.handler(new ChannelInitializer<SocketChannel>() {
						@Override
						public void initChannel(SocketChannel ch) throws Exception {
							ch.pipeline()
									.addLast(new ByteToCInt4MessageDecoder(tidMap))
									.addLast(new CInt4MessageToByteEncoder(tidMap))
									.addLast(new SetPointHandler(params));
						}
					});

			ChannelFuture f = bootstrap.connect(params.get("server-host"), Integer.parseInt(params.get("server-port")))
					.sync();
			
			f.channel().closeFuture().sync();

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			workerGroup.shutdownGracefully();
		}
	}
}
