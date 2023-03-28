package com.github.apuex.cmcc.cint4.utility;

import java.util.concurrent.TimeUnit;

import com.github.apuex.cmcc.cint4.HeartBeat;

import io.netty.channel.ChannelHandlerContext;
import io.netty.util.ReferenceCountUtil;
import io.netty.util.concurrent.ScheduledFuture;

public class ServerHandler extends io.netty.channel.ChannelInboundHandlerAdapter {
	@SuppressWarnings("rawtypes")
	ScheduledFuture heartBeatTask;
	@Override
	public void channelActive(ChannelHandlerContext ctx) throws Exception {
		System.out.printf("SYN %s : connection established.\n", ctx.channel().remoteAddress());
		heartBeatTask = ctx.executor().scheduleWithFixedDelay(() -> {
 			HeartBeat msg = new HeartBeat();
 			System.out.printf("SND %s : %s\n", ctx.channel().remoteAddress(), msg);
			ctx.writeAndFlush(msg);
		}, 5, 5, TimeUnit.SECONDS);
		
		ctx.fireChannelActive();
	}

	@Override
	public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
		System.out.printf("RCV %s : %s\n", ctx.channel().remoteAddress(), msg);
		ReferenceCountUtil.release(msg);
	}

	@Override
	public void channelInactive(ChannelHandlerContext ctx) throws Exception {
		System.out.printf("RST %s : connection closed.\n", ctx.channel().remoteAddress());
		heartBeatTask.cancel(false);
		ctx.fireChannelInactive();
	}

	@Override
	public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) throws Exception {
		System.out.printf("ERR %s : %s\n", ctx.channel().remoteAddress(), cause);
		ctx.fireExceptionCaught(cause);
	}
}
