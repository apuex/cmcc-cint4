package com.github.apuex.cmcc.cint4.utility;

import java.nio.ByteBuffer;

import io.netty.buffer.ByteBuf;
import io.netty.channel.ChannelHandlerContext;
import io.netty.util.ReferenceCountUtil;

public class ServerHandler extends io.netty.channel.ChannelInboundHandlerAdapter {
	    @Override
	    public void channelActive(ChannelHandlerContext ctx) throws Exception {
	    	System.out.printf("SYN%s : connection established.\n", ctx.channel().remoteAddress());
	        ctx.fireChannelActive();
	    }

	    @Override
	    public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
	    	if(msg instanceof ByteBuf) {
	    		ByteBuf nettyByteBuf = (ByteBuf) msg;
	    		ByteBuffer buf = nettyByteBuf.nioBuffer();
	    		handleRead(ctx, buf);
	    	} else if(msg instanceof ByteBuffer) {
	    		ByteBuffer buf = (ByteBuffer)msg;
	    		handleRead(ctx, buf);
	    	} else {
	    		System.out.printf("RCV%s : %s ", ctx.channel().remoteAddress(), msg);
	    	}
	        //ctx.fireChannelRead(msg);
	    	ctx.writeAndFlush(ctx.alloc().buffer(8).writeBytes("ok.\n".getBytes()));
	        ReferenceCountUtil.release(msg);
	    }

	    private void handleRead(ChannelHandlerContext ctx, ByteBuffer buf) {
	        System.out.printf("RCV%s : [ ", ctx.channel().remoteAddress());
	      	for(; buf.remaining() != 0;) {
	          System.out.printf("%02X ", 0xff & buf.get());
	      	}
	        System.out.printf("]\n");
	    }
	    
	    @Override
	    public void channelInactive(ChannelHandlerContext ctx) throws Exception {
	    	System.out.printf("RST%s : connection closed.\n", ctx.channel().remoteAddress());
	        ctx.fireChannelInactive();
	    }
	    
	    @Override
	    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause)
	            throws Exception {
	    	System.out.printf("ERR%s : %s\n", ctx.channel().remoteAddress(), cause);
	        ctx.fireExceptionCaught(cause);
	    }
}
