package com.github.apuex.cmcc.cint4.utility;

import io.netty.channel.ChannelHandlerContext;
import io.netty.util.ReferenceCountUtil;

public class ModifyPasswdHandler extends io.netty.channel.ChannelInboundHandlerAdapter {

	@Override
	public void channelActive(ChannelHandlerContext ctx) throws Exception {
		System.out.printf("SYN %s : connection established.\n", ctx.channel().remoteAddress());
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
		ctx.fireChannelInactive();
	}

	@Override
	public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) throws Exception {
		System.out.printf("ERR %s : %s\n", ctx.channel().remoteAddress(), cause);
		ctx.fireExceptionCaught(cause);
		ctx.close();
	}
}
