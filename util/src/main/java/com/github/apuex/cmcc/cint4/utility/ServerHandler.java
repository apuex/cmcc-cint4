package com.github.apuex.cmcc.cint4.utility;

import java.util.concurrent.TimeUnit;

import org.slf4j.LoggerFactory;

import com.github.apuex.cmcc.cint4.HeartBeat;

import ch.qos.logback.classic.Logger;
import io.netty.channel.ChannelHandlerContext;
import io.netty.util.ReferenceCountUtil;
import io.netty.util.concurrent.ScheduledFuture;

public class ServerHandler extends io.netty.channel.ChannelInboundHandlerAdapter {
	private static final Logger logger = (Logger) LoggerFactory.getLogger(ServerHandler.class);
	@SuppressWarnings("rawtypes")
	ScheduledFuture heartBeatTask;
	private int serialNo = 0;

	@Override
	public void channelActive(ChannelHandlerContext ctx) throws Exception {
		logger.info(String.format("[%s] SYN : connection established.", ctx.channel().remoteAddress()));
		heartBeatTask = ctx.executor().scheduleWithFixedDelay(() -> {
			HeartBeat msg = new HeartBeat(++serialNo);
			ctx.writeAndFlush(msg);
			logger.info(String.format("[%s] SND : %s", ctx.channel().remoteAddress(), msg));
		}, 5, 5, TimeUnit.SECONDS);
		
		ctx.fireChannelActive();
	}

	@Override
	public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
		logger.info(String.format("[%s] RCV : %s", ctx.channel().remoteAddress(), msg));
		ReferenceCountUtil.release(msg);
	}

	@Override
	public void channelInactive(ChannelHandlerContext ctx) throws Exception {
		logger.info(String.format("[%s] RST : connection closed.", ctx.channel().remoteAddress()));
		heartBeatTask.cancel(false);
		ctx.fireChannelInactive();
	}

	@Override
	public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) throws Exception {
		logger.info(String.format("[%s] ERR : %s", ctx.channel().remoteAddress(), cause));
		ctx.fireExceptionCaught(cause);
	}
}
